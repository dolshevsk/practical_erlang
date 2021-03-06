# Инфраструктура: OTP фреймворк, rebar, релизы

OTP фреймворк важная часть эрланг. Настолько важная, что язык на самом
деле называется не просто Erlang, а Erlang/OTP. Проекты на эрланг без
использования OTP встречаются крайне редко, и, обычно, это небольшие
библиотеки.

Рассмотренные на предыдущих уроках gen\_server, supervisor и
application являются частью OTP.

OTP -- это аббревиатура, которая читается как Open Telecom Platform.
То есть, платформа для создания проектов в области Telecom.  Название
не совсем правильное, на самом деле это платформа общего назначения,
для создания любых проектов.

OTP включает:
- приложения, формирующие базовое окружение: kernel, stdlib, erts, sasl;
- другие приложения, реализующие полезные функции: crypto, ssh, ssl, mnesia, wx, xmerl и другие;
- поведения: gen\_server, supervisor, application, gen\_fsm, gen\_event;
- ряд библиотек: lists, dict, maps, rand, re, timer и другие;
- инструменты для работы над проектом: компилятор, отладчик, профилировщик, статический анализатор и другие.

За 20 лет своего существования OTP проверен во многих
высоко-нагруженных и распределенных проектах. Так что гораздо лучше
полагаться на уже имеющиеся средства: gen\_server, supervisor,
библиотеки и т.д., чем пытаться реализовать свои аналогичные.


## Типичная структура проекта

Для эрланг есть рекомендуемая структура проекта. На эту структуру рассчитаны
инструменты, входящие в состав OTP. В первую очередь компилятор, и скрипты,
собирающие релизы.


### Минимальная структура

```
├─ ebin
│  ├── my_app.app
│  └── my_app.beam
└─ src
   └── my_app.erl
```

Самый простой OTP проект может иметь только две папки:
**src** для исходников и **ebin** для скомпилированных модулей.
В ebin также находится и файл ресурсов приложения.


### Проект из одного приложения

```
├─ ebin
│  ├── my_app.app
│  └── my_app.beam
├─ include
│  └── my_app.hrl
├─ priv
│  └── some.resource
└─ src
   └── my_app.erl
```

Кроме этого к стандартным папкам еще относятся:
**include** для заголовочных файлов,
**priv** для хранения каких-либо ресурсов, необходимых проекту
(файлы с данными, ssl-сертификаты, схемы валидации и т.д.).


# rebar3

[Getting Started](https://www.rebar3.org/docs/getting-started)

установить и запустить rebar3

скачать https://www.rebar3.org

поставить флаг +х,
```
chmod +x rebar3
```

положить в path или запустить по абсолютному/относительному пути,
```
path/to/rebar3 new release my_cool_project
```

положить в проект и запускать оттуда
```
cp path/to/rebar3 my_cool_project/
cd my_cool_project
./rebar3
```

Посмотреть структуру сгенерированного проекта и все файлы.

Добавить конфигурацию приложения в sys.config
```
  { my_cool_project, [
    {key1, 42},
    {key2, "Hello"}
  ]},
```

Собрать и запустить проект
```
./rebar3 compile
./rebar3 shell
```
Посмотреть observer.
Проверить конфигурацию.

Добавить модуль gen_server (из приготовленного шаблона worker_tpl.erl), запустить его под супервизором.

Отключить шум в консоли, который генерирует sasl (сообщения о запуске супервизоров и т.д.)
В sys.config добавить
```
  {sasl, [
    {errlog_type, error}
  ]},
```

Передать в gen_server аргументы из супервизора. Сформировать стейт с этими аргументами.
Запустить два gen_server с разными аргументами.

Добавить в rebar.config зависимость lager
```
{erl_opts, [
    debug_info,
    {parse_transform, lager_transform}
]}.

{deps, [
    {lager, "3.6.9"}
]}.
```

Скачать эту зависимость.
```
./rebar3 get-deps
```

Рассказать про проблемы в управлении зависимостями, версии зависимостей, роль rebar.lock

Использовать lager, вывод в консоль и в файл при дефолтной конфигурации.

Изменить конфигурацию lager, получить цветной вывод на консоль.
примеры конфигурации взять тут: https://github.com/erlang-lager/lager

Собрать и запустить релиз.

Рассказать про профили в конфигурации rebar.


## Проблемы в управлении зависимостями

Иногда в проектах на github можно видеть rebar.config, где Revision для
зависимостей указан как "HEAD" или "master" или "". Все эти варианты
значат одно и то же -- брать самую последнюю версию.

Так делать не рекомендуется. Пока вы работаете над своим проектом,
авторы библиотек не дремлют, и тоже работают с ними. Они могут
изменить API или реализацию библиотеки.  И может оказаться, что ваш
код, прекрасно работавший вчера, сегодня уже не работает.  Еще хуже,
если это выяснится уже после разворачивания на боевом сервере.
Поэтому рекомендуется всегда указывать тэг или коммит для всех
зависимостей, которые вы используете.

К сожалению, на этом проблемы не заканчиваются. Еще существуют
транзитивные зависимости.  Например, вы используете библиотеку А, а та
использует библиотеку Б.  Для библиотеки А вы в своем rebar.config
укажете тэг. Но может оказаться, что в rebar.config библиотеки А
указана зависимость от Б без тэга.

И тут нет простого выхода из ситуации. Либо жить с этим и надеяться на
лучшее, что годится для любительского проекта. Либо делать форк
библиотеки А и исправлять в ней rebar.config. Для серьезного проекта
вполне разумно сделать форки всех зависимостей, в том числе
транзитивных.

Далее проблемы рождают уже сделанные форки. Оригинальные библиотеки
развиваются авторами дальше -- в них исправляются баги, добавляются новые
функции.  Вам нужно либо отслеживать эти изменения и мержить их в
свои форки, либо игнорировать их.


## Релизы

Релизы подразумевают, что вашему проекту, кроме эрланг, больше ничего
не нужно.  Часто это не так. Проекту может быть нужна база данных, или
какой-нибудь внешний сервис. Или в нем есть код на других языках
программирования.  Тогда при развертывании на боевом сервере нужно
устанавливать и настраивать базу данных, другие языки
программирования, библиотеки на этих языках, и т.д.

Есть много вариантов, как это можно сделать. Тут и **rpm**/**deb** пакеты,
и инструменты типа [Fabric](http://www.fabfile.org/),
и [Docker](https://www.docker.com/) контейнеры.

Каждая команда делает это по-своему, от конкретных рекомендаций я воздержусь.

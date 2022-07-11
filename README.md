# Test IST

[![Erlang Versions](https://img.shields.io/badge/Supported%20Erlang%2FOTP-23.0%20to%2025.0-blue)](http://www.erlang.org)

1. [db crud](#db-crud)
2. [map reduce](#map-reduce)

## DB CRUD

Create, read, update and delete are the four basic operations of persistent storage.

db crud usage:

```bash
$ db:start().
$ db:new("Russia").
$ db:new("USA").
$ db:create({1,"Kostya", "Izhevsk"}, "Russia").
$ db:create({2,"Elena", "Saint-Peterburg"}, "Russia").
$ db:create({3,"Andrey", "Moskow"}, "Russia").
$ db:create({1,"John", "New Yourk"}, "USA").
$ db:create({2,"Eva", "Las Vegas"}, "USA").
$ db:create({3,"Andrey", "Las Vegas"}, "USA").
$ db:read(1, "Russia").
$ db:read(3, "Russia").
$ db:read(3, "USA").
$ db:update({3, "Andrey V.", "Moskow"}, "Russia").
$ db:update({3, "Andrey M.", "Las Vegas"}, "USA").
$ db:read(3, "Russia").
$ db:read(3, "USA").
$ db:read(2, "Russia").
$ db:delete(2, "Russia").
```

## MAP REDUCE

The program counts the number of words in files and return map words.

map_reduce usage:

```bash
$ map_reduce:start(["war_and_peace1.txt", "war_and_peace2.txt"]).
$
$ {ok, Files} = file:list_dir(".").
$ FunFilter = fun(X) -> case binary:split(list_to_binary(X), [<<".">>]) of
$   [_Name, <<"txt">>] -> true;
$   _ -> false
$ end
$ end.
$ FilesTxt = lists:filter(FunFilter, Files),
$ start(FilesTxt).
```
# One Log

## Install

* Requires installing [stack]() and [maven]() (which implies installing Java)
* To build all the needed components on Unixish systems:

```
$ ./Build.hs build
```

## Run

To run all components on a Unixish system:

```
$ ./Build.hs run
{"log":{"message":"starting java -jar ./pet-store-payment/target/pet-store-payment-1.0-SNAPSHOT.jar server payment-conf.yaml"},"node":"driver"}
{"log":{"message":"starting stack exec pet-store-server -- Dev 9090 localhost 8080"},"node":"driver"}
# ... more logs
^C
```

This spawns the PetStore and Payment processes, wrapping their `stdout` and `stderr` into JSON-formatted log entries.
Typing `Ctrl + C` should stop both processes.

The format of the logs is simple:

* `log`: The output from the process. If this output is already a JSON object, then this field contains an object, otherwise the output text
  is wrapped into an object with field `message`
* `node`: The name of the process from which this log entry comes from. The parent process is named `driver`

## Test

PetStore comes with a *driver* that uses a `Model` to generate tests representing user actions. This driver can be run
against the server:

```
$ stack exec driver-petstore -- localhost 9090
+++ OK, passed 100 tests.
```

# TODO

* [x] base client/server app with textual traces in Haskell
* [x] add another service in Java
* [x] connect the 2 services
* [x] build script
* [x] run script
* [x] structure logging in JSON
* [x] merge logs from 2 services by tailing containers
* [ ] add system level logs
* [ ] add network related logs
* [ ] add opentracing IDs to logs
* [ ] add docker logs
* [ ] colorize logs according to source
* [ ] store logs in kafka
* [ ] apply rational transducer one logs to produce per-connection analysis
* [ ] flamegraph
* [ ] reinject logs into application for replay

# Plan

## Au commencement

* Une appli client/server
* On lance l'appli et on tail les logs -> du texte de base, des "stack trace"

## Structurer les logs

* Partir de l'appli avec des logs non structurés -> structurer les logs -> avoir un objet log qui a une structure et ne pas dumper du texte arbitraire
* On obtient une matière brute sur laquelle on va pouvoir travailler par la suite -> déjà un résultat utile
* On peut faire des query/filtrage avec jq ! -> richesse de traitement possible, information pas "écrasée"
* On peut parser les logs facilement

## Multiplier les sources de logs (rajouter des traces petit à petit)

### Merger "les" logs des services -> considérer TOUS les logs de ton application

* Ajouter un autre service avec des logs structurés
* Fusionner les 2 logs dans la console en tailant 2 containers
* jaeger/opentracing → traces cross-services

### Merger les logs du système

* Iostat -w 1 → Json
* tools/perf

### Code couleur pour la provenance des logs

### Stocker  les logs

* Les logs c'est pas un coût pour ton application mais une donnée précieuse  → distribuer le stockage
* Pousser les logs dans kafka

## Analyser des logs

### Visualisation

* Reconstruire un état à partir des traces de log → traces → State Machine
  * Pour analyser de la perf structure (flame graph)
  * Pour les tests
  * Reconstruire une "grammaire" à partir des logs, ML, automates probabilistes
* Replay
  * Mais en fait, si on a une appli event sourcé, les logs applicatifs sont gratos et on peut les rejouer

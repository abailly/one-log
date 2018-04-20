# TODO

* [x] base client/server app with textual traces in Haskell
* [x] add another service in Java
* [ ] structure logging in JSON
* [ ] merge logs from 2 services by tailing containers
* [ ] add opentracing IDs to logs
* [ ] add system level logs
* [ ] add network related logs
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

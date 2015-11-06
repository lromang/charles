#! /bin/bash

for i in $(cat ../../data/catalogs.json | awk -F '/' '{print $4}' ); do echo "adela.datos.gob.mx/$i/catalogo.json" >> ../../data/dependencies.txt ;done


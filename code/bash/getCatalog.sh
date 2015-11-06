#! /bin/bash

curl -sL adela.datos.gob.mx/api/v1/catalogs | jq '.' > ../../data/catalogs.json


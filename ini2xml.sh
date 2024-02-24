#!/bin/bash

# Verifica se o arquivo .ini foi especificado como argumento
if [ -z "$1" ]; then
  echo "Uso: $0 arquivo.ini"
  exit 1
fi

# Define o nome do arquivo .ini e .xml
ini_file="$1"
xml_file="${ini_file%.ini}.xml"

# Verifica se o arquivo .ini existe
if [ ! -f "$ini_file" ]; then
  echo "Arquivo $ini_file não encontrado"
  exit 1
fi

# Cria o arquivo .xml e escreve o cabeçalho
echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" > "$xml_file"
echo "<config>" >> "$xml_file"

# Lê o arquivo .ini e escreve os valores no arquivo .xml
while IFS='=' read -r key value; do
  if [[ $key != \#* && $key != \;* && ! -z $key ]]; then
    echo "  <${key//[\[\] ]}>${value//[\[\] ]}</${key//[\[\] ]}>" >> "$xml_file"
  fi
done < "$ini_file"

# Fecha a tag raiz
echo "</config>" >> "$xml_file"


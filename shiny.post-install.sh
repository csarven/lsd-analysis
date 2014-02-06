#!/bin/bash
sed -i 's/__assets__/\/__assets__/' /usr/lib/node_modules/shiny-server/R/SockJSAdapter.R

sudo mkdir -p /etc/shiny-server/
sudo cp server-config.conf /etc/shiny-server/

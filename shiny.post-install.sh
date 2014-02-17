#!/bin/bash
sed -i 's/'\''__assets__/'\''\/__assets__/g' /usr/lib/node_modules/shiny-server/R/SockJSAdapter.R

ln -s /usr/local/lib/R/site-library/shiny/www/shared/shiny.js www/shiny/shiny.js
ln -s /usr/local/lib/R/site-library/shiny/www/shared/jquery.js www/shiny/jquery.js

mkdir -p /var/shiny-server/www/lsd-analysis/www/plots
mkdir -p /var/shiny-server/www/lsd-analysis/www/csv

sudo chgrp -R shiny /var/shiny-server/www/lsd-analysis/www
sudo chmod -R g+wx /var/shiny-server/www/lsd-analysis/www
sudo chgrp -R shiny /var/shiny-server/log
sudo chmod g+wx /var/shiny-server/log

sudo mkdir -p /etc/shiny-server/
sudo ln -s /var/shiny-server/www/lsd-analysis/shiny-server.conf /etc/shiny-server/shiny-server.conf

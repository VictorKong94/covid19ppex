# Define some variables
USER="admin_is_the_default_in_aws"
PASSWORD="save_this_password_or_you_will_need_to_reset_it"
HOST="XXX.XXXXXXXXXXXX.XX-XXXX-X.rds.amazonaws.com"
SCHEMA="covid19ppex"
KEY="32UpperLoweCaseLettersAndDigits"

# Finish setting up R and Shiny Server
configure-R:
	# Specify package installation protocol
	echo "" >> /usr/lib/R/etc/Rprofile.site
	echo "## Configure package installation protocol for all users" >> /usr/lib/R/etc/Rprofile.site
	echo "options(download.file.method = \"libcurl\")" >> /usr/lib/R/etc/Rprofile.site
	# Use the default configuration of Shiny Server
	/opt/shiny-server/bin/deploy-example default

# Install packages specific to our Shiny application
install-dependencies:
	# Linux packages
	apt-get install -y libmariadb-client-lgpl-dev
	apt-get install -y libsodium-dev
	apt-get install -y libcurl4-openssl-dev
	apt-get install -y libssl-dev
	apt-get install -y libv8-dev
	# R packages
	su - -c "R -e \"install.packages('shinydashboard')\""
	su - -c "R -e \"install.packages('shinyjs')\""
	su - -c "R -e \"install.packages('data.table')\""
	su - -c "R -e \"install.packages('DT')\""
	su - -c "R -e \"install.packages('lubridate')\""
	su - -c "R -e \"install.packages('DBI')\""
	su - -c "R -e \"install.packages('httr')\""
	su - -c "R -e \"install.packages('jose')\""
	su - -c "R -e \"install.packages('RMySQL')\""
	su - -c "R -e \"install.packages('sodium')\""
	su - -c "R -e \"install.packages('V8')\""
	su - -c "R -e \"install.packages('remotes')\""
	su - -c "R -e \"remotes::install_version('dplyr', '0.8.4', repos = 'https://demo.rstudiopm.com/cran/__linux__/xenial/latest')\""

# Secure Shiny Server with SSL
configure-ssl:
	# Generate a cryptographic key
	openssl genrsa -out /etc/ssl/private/apache.key 2048
	# Create an SSL certificate: "Common Name" should be the public DNS name
	openssl req -new -x509 -key /etc/ssl/private/apache.key -days 365 -sha256 -out /etc/ssl/certs/apache.crt
	# Install Apache HTTPS web server
	apt-get install -y apache2
	apt-get install -y aptitude
	aptitude install -y build-essential libxml2-dev
	# Install the SSL and proxy modules in Apache
	a2enmod ssl proxy proxy_ajp proxy_http rewrite deflate headers proxy_balancer proxy_connect proxy_html
	# Configure Apache to forward https calls to Shiny Server port
	echo "<VirtualHost *:*>" > /etc/apache2/sites-enabled/000-default.conf
	echo "	SSLEngine on" >> /etc/apache2/sites-enabled/000-default.conf
	echo "	SSLCertificateFile /etc/ssl/certs/apache.crt" >> /etc/apache2/sites-enabled/000-default.conf
	echo "	SSLCertificateKeyFile /etc/ssl/private/apache.key" >> /etc/apache2/sites-enabled/000-default.conf
	echo "	ProxyPreserveHost On" >> /etc/apache2/sites-enabled/000-default.conf
	echo "	ProxyPass / http://0.0.0.0:3838/" >> /etc/apache2/sites-enabled/000-default.conf
	echo "	ProxyPassReverse / http://0.0.0.0:3838/" >> /etc/apache2/sites-enabled/000-default.conf
	echo "	ServerName localhost" >> /etc/apache2/sites-enabled/000-default.conf
	echo "</VirtualHost>" >> /etc/apache2/sites-enabled/000-default.conf
	# Restart Apache
	systemctl restart apache2

# Specify environment variables
set-env-vars:
	# Helps the application connect to the database
	echo "DATABASE_URL=mysql://$USER:$PASSWORD@$HOST:3306/$SCHEMA" > /srv/shiny-server/src/.Renviron
	# For producing encrypted tokens to allow easy login
	echo "AES_KEY=$KEY" >> /srv/shiny-server/src/.Renviron

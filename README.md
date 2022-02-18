# covid19ppex

Shiny app hosted on Open Source Shiny Server. `covid19ppex` provides a GUI-based tool for associated clinics to coordinate exchanges of personal protective equipment (PPE).

### Requirements

- Server running a Linux distribution
- MySQL database

### Server Set Up

This project was originally hosted on an AWS EC2 instance running Ubuntu. The instructions below assume the user is running macOS locally on a secure network.

1. Launch an instance from the AWS EC2 dashboard. These instructions outline the workflow for Ubuntu Server 18.04 LTS. You may use the default selections until you get to "Step 6: Configure Security Group". Here you'll want to use the following specifications:
   - Rule 1: { Type: SSH, Source: My IP }.
   - Rule 2: { Type: HTTPS, Source: Custom 0.0.0.0/0 }.

You'll be prompted to create a key pair. We created a new one and named it "shiny-server". This will download a `*.pem.txt` file to your hard disk.

2. Open your terminal and run the following commands to ssh into the server:

```bash
# Replace this with your instance's public DNS
EC2_PUBLIC_DNS="ec2-XXX-XXX-XXX-XXX.XX-XXXX-X.compute.amazonaws.com"

cd
mv Downloads/shiny-server.pem.txt .ssh/shiny-server.pem
chmod 400 .ssh/shiny-server.pem
ssh -i '.ssh/shiny-server.pem' ubuntu@$EC2_PUBLIC_DNS
```

3. Once you've successfully entered the server, we'll begin by installing R and Shiny Server:

```bash
# Gain access to root
sudo -i

# OS updates
apt-get update

# Install R
apt-get install -y r-base
apt-get install -y r-base-dev

# Download and install Shiny Server
su - -c "R -e \"install.packages('shiny')\""
apt-get install -y gdebi-core
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.13.944-amd64.deb
gdebi shiny-server-1.5.13.944-amd64.deb
```

4. Next, we'll clone this GitHub repository to the newly created `/srv/shiny-server/` directory:

```bash
git init
git remote add origin https://github.com/VictorKong94/covid19ppex.git
git pull origin master
```

5. This step makes use of the `Makefile` in the Git repository to finish configuring R and Shiny Server, and installing dependencies specific to our application. Note that this file is written for Ubuntu, and you may need to edit it if your server is running a different Linux distribution.

```bash
make configure-R
make install-dependencies
make configure-ssl
```

6. We recommend taking a break here to complete the **Database Set Up** protocol below. The following step assume the reader has completed at least up to step 2 of the **Database Set Up**.

7. Reopen the included `Makefile` and set the `KEY` variable equal to a string of up to 32 randomly-generated upper- and lowercase letters, and numbers. Only after you've specified those variables, run the following command:

```bash
make set-env-vars
```

8. The database application should now be online and accessible at `https://[EC2_PUBLIC_DNS]/covid19ppex/src/`. You may move or rename the `src/` directory to alter the url. Log in using the "Chief Admin" profile to begin administrating  other user profiles. It's recommended to change the password for this profile to something secure.


### Database Set Up

This project used an AWS RDS instance running MySQL. The instructions below assumes the reader will do the same:

1. Create a database from the AWS RDS dashboard. Select "MySQL" when prompted for the engine type. You may use "Easy Create" to use recommended configuration options. The DB instance identifier, master username, and master password are left to the reader's discretion. When ready, create the database.

2. Open the included `Makefile` and specify the `USER`, `PASSWORD`, and `HOST` variables for your newly-created database. Place your entries within the quotations. In particular, the `HOST` variable is given by the "Endpoint", which can be found under the "Connectivity & security" tab once the database has been created.

3. Configure the database's security group's inbound rules using the following specifications:
   - Rule 1: { Type: MYSQL/Aurora, Source: My IP }.
   - Rule 2: { Type: MYSQL/Aurora, Source: Custom (Server's Private IP address) }.

Save your changes when finished.

4. Connect to the database using the method of your choice, and run the code included in the `create_database.sql` file to produce the necessary tables to sustain the application.


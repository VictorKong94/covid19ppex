# Changelog

All notable changes to this project will be documented in this file.

## [3] - 2020-06-26

### Added

- A `README.md` file with step-by-step instructions for launching a new
  application instance.
- A `create_database.sql` file with code to create the database schema and
  tables necessary to sustain an application instance.
- Class for active tab in top-level navigation bar.
- Enter key can be used to submit authentication request.
- Error handling for case when active user is not a member of any clinics.
- Functionality to allow users to stay logged in with a browser token.
- Functionality to organize groups of clinics for reporting purposes.
- Super-admin role and granted privileges:
  - Creating new user accounts.
  - Resetting user passwords.
  - Editing equipment lookup table.

### Changed

- Wording of "collaborators" to "staff".
- Wording of "zip code" to "ZIP".

### Removed

- User experience survey.
- Sidebar panel.

## [2] - 2020-04-23

### Added

- This `CHANGELOG.md` file to elaborate on changes made to this project.
- An `init.sh` file with commands to set up and configure Shiny Server to run
  this application on an Ubuntu server instance.
- A `packages.txt` file that lists R packages necessary to run this application.
- The `www/` directory for `*.css` and `*.js` files. This currently contains:
  - A `stylesheet.css` file to customize elements of the user interface.
  - An `inactivity.js` file to trigger a session timeout after 180 seconds of
    user inactivity.
- A basic, downloadable report that is generated within the application to
  summarize the current status of PPE needs.

### Changed

- Changed the `requests` table to `posts`, to allow tracking of both requests
  for PPE and offers of surplus PPE.
- Modified some SQL queries to work with a new MySQL database instance hosted on
  Amazon Web Services.

### Removed

- The `Aptfile`, `init.R`, and `run.R` files have all been removed since the
  application was moved from Heroku to an Amazon Web Services EC2 instance.

## [1] - 2020-04-19

### Added

- A set of `server.R`, `ui.R`, and `global.R` files to generate a database
  application meant to be used as a forum for clinics to request personal
  protective equipment (PPE) from other clinics that have equipment to spare.
- Some R packages have binary dependencies; an `Aptfile` lists such Ubuntu
  packages that need to be installed for Heroku to compile a working slug.
- An `init.R` file that's executed when Heroku compiles a slug and installs R
  packages necessary to run the application.
- A `run.R` file that imports the Shiny package and specifies the `PORT`
  environment variable, provided by Heroku, that's used to configure Shiny
  accordingly.
- A `README.md` file that is currently blank.

formal-aspects-docker-runner

## Getting started

0. Download, install and run Docker from https://docs.docker.com/get-docker/
1. Download rsltc_2.6.1-1_i386.deb from ` RAISE Tools / rsltc-for-Linux / rsltc_2.6.1-1_i386 ` into project folder
2. Build and start container with `docker-compose up -d`
3. Attach to the container with `docker-compose exec rsl bash`
4. You can use `rsltc` and `sml` commands
5. When you're done, exit and stop container with `docker-compose down`

Optionaly you can use VSCode for coding and running commands

Credit for the docker files: https://github.com/danielgaldev/

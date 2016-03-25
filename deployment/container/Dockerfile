# Dockerfile that builds AlphaSheets on top of fpco/stack-build

FROM fpco/stack-build
MAINTAINER ritesh@alphasheets.com

# Update package manager and install all needed packages
# Running all installs with apt-get update uses cache busting
RUN add-apt-repository ppa:chris-lea/redis-server
RUN apt-get update && apt-get install -y \
  redis-server \
  libboost-all-dev \
  libzmq3-dev \
  libtool \
  pkg-config \
  build-essential \
  autoconf \
  automake

# Start and configure Redis with password
RUN /etc/init.d/redis-server start && redis-cli config set requirepass $6$saltsalt$Uo5kzWNTGQ2076QrWJ82kMi9kDBJbDLsHhWED3z07aMh5FhdziAKu8RhfoJo.bC0uuXRAc2tvhKIO5OeoHB/X1

# Add zeromq and libsodium install script to container, and run it
ADD installs.sh /usr/local/bin/installs.sh
RUN sudo chmod +x /usr/local/bin/installs.sh
RUN ./usr/local/bin/installs.sh

# Install R package dependencies
RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile&& \
    Rscript -e "install.packages('rjson')" && \
    Rscript -e "install.packages('ggplot2')" && \
    Rscript -e "install.packages('party')" && \
    Rscript -e "install.packages('jpeg')"

# Expose port 5000, 8000, and 9000 for backend, static server, and file-input server
EXPOSE 5000
EXPOSE 8000 
EXPOSE 9000

# Environment variable to enable if you want to pull from github and bust the cache
ENV test1 test3
# Make an alphasheets folder inside the home directory
# Pull from the builds folder using OAuth
# The user for this OAuth is favoriteshirt, who only has access to the builds repo
RUN cd ../home && \
    rm -rf alphasheets && \
    mkdir alphasheets && \
    cd alphasheets && \
    git init && \
    git pull https://fa848255af68ed0e25ffe52a241be236781272bd@github.com/ooblahman/alphasheets-builds.git

# Copy the startup script in the right place
# This startup script starts redis, then calls execute.sh as a non-root user
ADD startup.sh /usr/local/bin/startup.sh
RUN sudo chmod +x /usr/local/bin/startup.sh

# Add the execute script in the right place (runs the executables)
ADD execute.sh /usr/local/bin/execute.sh

# Permissions for demo (non-root user)
# Own, read, and execute /home/alphasheets, write to /server/static/X (but not executable) (for images)
# Own and execute execute.sh
RUN useradd demo && \
    chown -R demo: /home/alphasheets/ && \
    chmod -R 500 /home/alphasheets/ && \
    chmod -R 700 /home/alphasheets/server/ && \
    chmod 500 /home/alphasheets/server/alphasheets-exe && \
    chmod 500 /home/alphasheets/server/rkernel-exe && \
    chown demo: /usr/local/bin/execute.sh && \
    chmod 500 /usr/local/bin/execute.sh

ENTRYPOINT ["/usr/local/bin/startup.sh"]


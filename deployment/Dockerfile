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

# Copy the startup script in the right place
# This startup script pulls from the github builds repo, and runs the executables.
ADD startup.sh /usr/local/bin/startup.sh
RUN sudo chmod +x /usr/local/bin/startup.sh

# Make an alphasheets folder inside the home directory
# Pull from the builds folder using OAuth
# The user for this OAuth is favoriteshirt, who only has access to the builds repo
RUN cd ../home && \
    rm -rf alphasheets && \
    mkdir alphasheets && \
    cd alphasheets && \
    git init && \
    git pull https://fa848255af68ed0e25ffe52a241be236781272bd@github.com/ooblahman/alphasheets-builds.git

# Install R package dependencies
RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile&& \
    Rscript -e "install.packages('rjson')" && \
    Rscript -e "install.packages('ggplot2')" && \
    Rscript -e "install.packages('party')" && \
    Rscript -e "install.packages('jpeg')"

# Expose port 80, which will be bound to port 80 on the host upon docker run, and
# run a startup script upon starting
EXPOSE 80
ENTRYPOINT ["/usr/local/bin/startup.sh"]


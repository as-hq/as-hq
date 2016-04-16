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
  automake \ 
  python-all-dev \
  libpng-dev \
  zlib1g-dev \
  libfreetype6-dev \
  python-dev \
  python-pip \
  python-numpy

# Add zeromq and libsodium install script to container, and run it
ADD installs.sh /usr/local/bin/installs.sh
RUN sudo chmod +x /usr/local/bin/installs.sh
RUN ./usr/local/bin/installs.sh

# Install Python dependencies 
RUN pip install six && \
    pip install numpy && \
    pip install matplotlib && \
    pip install pandas && \
    pip install yahoo_finance && \
    pip install -U pandasql && \
    pip install sqlalchemy && \
    pip install zmq && \
    pip install ipython && \
    pip install openpyxl && \
    pip install statistics

# Install R package dependencies
RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile&& \
    Rscript -e "install.packages('rjson')" && \
    Rscript -e "install.packages('ggplot2')" && \
    Rscript -e "install.packages('party')" && \
    Rscript -e "install.packages('jpeg')"
    
# Installing numpy gives a version mismatch in PyKernel. This fixes that.
RUN rm -rf /usr/lib/python2.7/dist-packages/numpy && \
    pip install numpy
    
RUN pip install scipy

# Expose port 5000, 8000, and 9000 for backend, static server, and file-input server
EXPOSE 5000
EXPOSE 8000 
EXPOSE 9000

# Add password to redis conf
ADD redis.conf /etc/redis/redis.conf


RUN git config --global user.email "you@example.com" && \
    git config --global user.name "Your Name"

# bust the cache from here on out, to c lean build from gitub always
ADD version /tmp/

ENV BRANCH "stable"

# Make an alphasheets folder inside the home directory
# Pull from the builds folder using OAuth
# The user for this OAuth is favoriteshirt, who only has access to the builds repo
RUN cd /home && \
    rm -rf alphasheets && \
    git clone --branch="$BRANCH" https://fa848255af68ed0e25ffe52a241be236781272bd@github.com/ooblahman/alphasheets-builds.git alphasheets

# Copy the startup script in the right place
# This startup script starts redis, then calls execute.sh as a non-root user
ADD startup.sh /usr/local/bin/startup.sh
RUN sudo chmod +x /usr/local/bin/startup.sh

# Develop python package as root
RUN cd /home/alphasheets/pylib && python setup.py develop

# Install sandboxR
RUN cd /home && \
    git clone https://github.com/RiteshRN/sandboxR sandboxR && \
    R CMD INSTALL sandboxR

# Add the execute script in the right place (runs the executables)
ADD execute.sh /usr/local/bin/execute.sh

# Own, read, and execute /home/alphasheets, write to /server/static/X (but not executable) (for images)
# Own and execute execute.sh
# The app will need the images and log directories, and we set their write
# permissions here
RUN useradd demo && \
    mkdir /home/alphasheets/server/static/images/ && \
    mkdir /home/alphasheets/rkernel/logs && \
    chown -R demo: /home/alphasheets/ && \
    chmod -R 500 /home/alphasheets/ && \
    chmod -R 700 /home/alphasheets/server/ && \
    chmod -R 777 /home/alphasheets/pykernel/logs/ && \
    chmod -R 777 /home/alphasheets/rkernel/logs/ && \
    chmod 500 /home/alphasheets/server/alphasheets-exe && \
    chmod 500 /home/alphasheets/rkernel/rkernel-exe && \
    chown demo: /usr/local/bin/execute.sh && \
    chmod 500 /usr/local/bin/execute.sh  

VOLUME /var/lib/redis

# Add launch-related DB things. The image files must be in sync with the dump file.
ADD dump.rdb /var/lib/redis/dump.rdb
ADD 0e44656e421b416fbea3405529185e54.png /home/alphasheets/server/static/images/0e44656e421b416fbea3405529185e54.png
ADD b3744441-f1c1-4aaf-b344-16c95d0a0b63.png /home/alphasheets/server/static/images/b3744441-f1c1-4aaf-b344-16c95d0a0b63.png

ENTRYPOINT ["/usr/local/bin/startup.sh"]
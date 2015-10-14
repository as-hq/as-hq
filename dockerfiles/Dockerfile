FROM fpco/stack-build:lts-2.14

# Install system dependencies.
RUN apt-add-repository ppa:marutter/rrutter \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
         libzmq3-dev \
         python-dev \
         python-pip \
         r-base \
         r-base-dev \
    && rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/redis/hiredis \
    && cd hiredis \
    && make && make install \
    && cd ..

# NOTE: this dockerfile does not describe all necessary dependencies.
# you still have to execute the setup instructions on the wiki in the container
# if you're building from this file. 

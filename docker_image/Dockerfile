FROM haskell:7.8
RUN apt-get update
RUN apt-get install -y apt-utils
RUN apt-get install -y\
    	    	      libfann-dev\
		      pkg-config\
		      wget\
		      g++\
		      make\
		      libcairo2-dev\
		      freeglut3-dev\
		      mesa-common-dev\
		      autoconf\
		      vim\
		      locate
RUN cabal update -v && cabal install alex happy cabal-install c2hs
RUN wget http://fltk.org/pub/fltk/1.3.3/fltk-1.3.3-source.tar.gz && tar xvf fltk-1.3.3-source.tar.gz && cd fltk-1.3.3 &&\
    ./configure --enable-gl --enable-cairo && make && make install
RUN mkdir /app
WORKDIR /app
COPY include/linker-config /usr/bin/
RUN chmod +x /usr/bin/linker-config
CMD ["bash"]
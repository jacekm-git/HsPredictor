FROM haskell:7.8
RUN apt-get update && apt-get install -y\
    	    	      libfann-dev\
		      pkgconf
RUN cabal update -v && cabal install alex happy
RUN mkdir /app
WORKDIR /app
ENTRYPOINT ["cabal"]
CMD ["--version"]
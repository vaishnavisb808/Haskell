# Use the existing Haskell image as our base
FROM haskell:8.10 as build
WORKDIR /app
#copy stack.yaml and .cabal file to build folder
COPY stack.yaml *.cabal /app/build/
RUN apt-get update
RUN apt-get install libpq-dev -y
#Build only dependencies
RUN cd build && stack build --only-dependencies --copy-bins --local-bin-path .
#Copy all the project files to build folder
COPY . /app/build
COPY ./static /app/build_output/static
#Build the project and Copy the generated executable to build_output folder
#in the work directory
ARG ENV
RUN if [ "$ENV" = "DEV" ]; \
    then \
        echo "DEV mode"; \
        cd build && stack build --fast  --ghc-options -O0 --copy-bins --local-bin-path ../build_output; \
    else \
        echo "PROD mode"; \
        cd build && stack build   --ghc-options -O2 --copy-bins --local-bin-path ../build_output; \
    fi
#remove the project files post build
RUN rm -r build



#Using ubuntu:latest as base
FROM ubuntu
WORKDIR /app
RUN apt-get update
RUN apt-get install libpq-dev -y
RUN apt-get install ca-certificates -y
#Copy the binary generated during build
COPY --from=build /app/build_output .
# Expose a port to run our application
EXPOSE 8080
# Run the executable
CMD ./vision-api-exe

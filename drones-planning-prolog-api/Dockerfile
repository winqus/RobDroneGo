# Use an official Ubuntu image as the base
FROM ubuntu:latest

# Set the working directory in the container
WORKDIR /app

# Install SWI-Prolog
RUN apt-get update && \
    apt-get install -y software-properties-common && \
    apt-add-repository ppa:swi-prolog/stable && \
    apt-get update && \
    apt-get install -y swi-prolog

# Copy the local code to the container
COPY ./src ./src

# Expose the port the app runs on
EXPOSE 4400

# Define the command to run the app
# ENTRYPOINT [“swipl”]
# CMD ["swipl", "/src/server.pl", “–user=daemon”, “–fork=false”, “–port=4400]
CMD ["swipl", "-s", "src/server.pl", "-g", "start_server(4400)"]

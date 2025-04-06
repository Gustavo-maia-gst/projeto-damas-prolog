FROM swipl:latest

WORKDIR /app

COPY . .

CMD ["swipl", "-s", "src/main.pl", "-g", "main", "-t", "halt"] 
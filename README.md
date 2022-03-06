# Deb's License Plate Game

My wife likes to play a game with license plates.

[Wordnik API documentation](https://developer.wordnik.com/docs#!/word/getScrabbleScore)

```
https://api.wordnik.com/v4/word.json/question/scrabbleScore?api_key=YOURAPIKEY
```

[algorithm - Algorithmic scoring based on time - Stack Overflow](https://stackoverflow.com/questions/21771670/algorithmic-scoring-based-on-time/21771707#21771707)

Word found

`https://api.wordnik.com/v4/word.json/fuzz/scrabbleScore?api_key=YOURAPIKEY`

```json
{
  "value": 25
}
```

Word not found

`https://api.wordnik.com/v4/word.json/fzu/scrabbleScore?api_key=YOURAPIKEY`

```json
{
  "statusCode": 404,
  "error": "Not Found",
  "message": "Not found"
}
```

Wordnik has an open-source [word list](https://github.com/wordnik/wordlist).
The latest version is [July 2021](https://raw.githubusercontent.com/wordnik/wordlist/main/wordlist-20210729.txt)
It is a list of strings in quotes, but not CSV or JSON.
Here are the steps to convert.

```shell
# Download the file
curl https://raw.githubusercontent.com/wordnik/wordlist/main/wordlist-20210729.txt -o wordlist.txt

# Copy to json
cp wordlist.txt wordlist.json

# Add commas to each line
sed '$!s/$/,/' wordlist.json

# Add an opening square bracket
sed -i '1 i\[' wordlist.json

# Add a closing bracket
sed -i "$ a ]" wordlist.json

# Format (optional)
npx prettier --write wordlist.json
```

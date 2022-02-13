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

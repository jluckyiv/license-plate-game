# Download the file
curl https://raw.githubusercontent.com/wordnik/wordlist/main/wordlist-20210729.txt -o wordlist.txt

# Copy to json
cp wordlist.txt ./src/wordlist.json

# Use gsed
if command -v gsed &> /dev/null
then
  alias sed=gsed
fi

# Add commas to the end of each line
sed -i '$!s/$/,/' ./src/wordlist.json

# Add an opening square bracket
sed -i '1 i\[' ./src/wordlist.json

# Add a closing bracket
sed -i '$ a ]' ./src/wordlist.json

# Clear the alias
if command -v gsed &> /dev/null
then
  unalias sed
fi

# Format (optional)
npx prettier --write ./src/wordlist.json

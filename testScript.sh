IFS=,

declare -A testMap

testLine="Blah,Bleh,Heh"

for word in $testLine; do
	testMap[$word]=$word
done

for word in "${!testMap[@]}"; do
        echo "$word"
done

echo ${testMap["asd"]}
echo ${testMap["Blah"]}
echo "${testMap[Blah]} YEAH BABY"

BEGIN {
	SN = "\n[source,c]\n----\n"
	EN = "\n----\n"
}

{
	if(gsub(/\(\*\*/, "", $0)) {
		buffer = buffer "\n" $0
		print SN buffer EN
		buffer = ""
		next
	}
	if(gsub(/\*\*\)/, "", $0)) {
		buffer = buffer "\n" $0
		print buffer
		buffer = ""
		next
	}
	buffer = buffer "\n" $0
}
END {
	print EN
}
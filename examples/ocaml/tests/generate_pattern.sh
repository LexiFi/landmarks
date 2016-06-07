#!/bin/bash

echo "type t ="
for ((i = 0; i < $1; i++)) do
	echo "| A$i"
done
echo "let of_string = function"
for ((i = 0; i < $1; i++)) do
	echo "| \"$i\" -> A$i"
done
echo "| _ -> assert false"

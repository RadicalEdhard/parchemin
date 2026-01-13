X		# Development guide and Roadmap to Parchemin 1.0

## Objectif
The goal of parchemin project is to make a command line tool that encode standard input to exchange format
(e.g intel hex, S-record, paquets )
## Commande line description
parchemin [options] <command> [ARGS]

## Tree

## Roadmad

| task 																														|finished |
|:---																																|:---				|
| Database parser																								|O				|	
| Implemtent good modular and reusable command line system |X				|
| Encoder (which decode and convert too)												|X 				|
| Mapping procedure (maybe above encoder)										|X				|
| Checker (with checksums algorithm support)										|X				|

*X=>  not implemented | V => Implemented | O => In progress*
### Options
|Option		|Description		|
|:--- 				|:--- 						|
|-h, --help	| Display help	|

### Commands 
|Command 	|Description  																|Implemented	|
|:---						|:---																						|:---:						|
|check				| Process and check CRC/checksums					|X							|
|convert 			| From format to other.												|X							|
|decode			| Extract data from structured file.						|X							|
|encode			| Convert input to structured format. 				|X							|
|map					| Make a memory map from structured file.	|X							|
|patch				| Update byte or value at address						|X							|

### Arguments

**check**

|Argument 	|Description															|Implemented |
|:---						|:---																				|:---:						|
| --algorithm 	| Specify the CRC	algorithm 							|X							|
|--fix					| Fix checksums and CRC. 								|X							|
|--format			| needed to check lines by lines					|X							| 
|--global			| Check the CRC.													|X							|
|--help				| print command documentation					|X							|
|--verify			| Checksums line by line (ihex, s-record). 	|X							|

**convert**

|Argument 	|Description															|Implemented |
|:---						|:---																				|:---:						|
| --format		| the wanted file or paquet format				|X							|
| --help 			| print command documentation 				|X							|
|--input				| the source file													|X							|
|--output			| the destination file											|X							|
|--spec				| the device or paquet specification			|X							|
			
**decode**

|Argument 	|Description															|Implemented |
|:---						|:---																				|:---:						|
| --help 			| print command documentation 				|X							|
|--input				| the source file													|X							|
|--output			| the destination file											|X							|
| --format		| the format to decode										|X							|

**encode**

|Argument 	|Description															|Implemented |
|:---						|:---																				|:---:						|
| --help 			| print command documentation 				|X							|
|--input				| the source file													|X							|
|--output			| the destination file											|X							|
|--spec				| the device or paquet specification			|X							|
| --format		| the format to encode										|X							|
| --map				| shortcut to map												|X							|

**map**

|Argument 	|Description															|Implemented |
|:---						|:---																				|:---:						|
| --help 			| print command documentation 				|X							|
| --format		| how to read and map this format				|X							|
|--input				| the source file													|X							|
|--output			| the destination file											|X							|

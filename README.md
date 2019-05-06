# aline

Replacement for `awk`/`cut`/`sed`, written in Rust. Never again struggle to remember which one does what you want or splits fields how you want, just switch to aline for all your line-parsing needs.

## Usage Examples

~~~bash
# default whitespace delimiter
$ echo 'field0 field1 field2' | al -f1
field1

# comma delimiter
$ echo 'field0,field1,field2' | al -d, -f1
field1

# multiple fields (keep same delimiter)
$ echo 'field0 field1 field2' | al -f0,2
field0 field2

# multiple fields (new format)
$ echo 'field0 field1 field2' | al -f0,2 -o csv
field0,field2

# input format: csv
$ echo '"field0","field1","field2"' | al -i csv -f1
field1

# input format: json
$ echo '{"a": "field0", "b": "field1", "c": "field2"} | al -i json -k b
field1

# input format: custom regex with numbered groups
$ echo 'we found field0 with code:field1' | al -i 'found (\S+) with code:(\S+)' -f 0,1
field0 field1

# custom output format (bash-style)
$ echo 'field0 field1 field2' | al -o '[$2] $0'
[field2] field0

# custom output format (rust-fmt-style)
$ echo 'field0 field1 field2' | al -o '[{2}] {0}'
[field2] field0
~~~

## Design

~~~
files || stdin
 => lines
     => parse([-d,-i])
 				 => {fields: vec[], keys: map[]}
							=> output([-o,-f,-k])
~~~

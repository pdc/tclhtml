# htmlProcs.tcl --
#  Tcl library for generating HTML/XML documents.
#  This replaces the definitions in htmlBase.tcl.

# We make our definitions in a namespace `html'.
#  We follow the slightly odd convention of starting private names
#  with capital letters, as described in Sun's Tcl coding standards.

namespace eval html {
    set infos(id) {$Id: htmlProcs.tcl,v 1.8 2004/03/22 23:18:30 pdc Exp $}
    set infos(versionMajor) 5
    set infos(version) $infos(versionMajor).[lindex $infos(id) 2]
    if {[llength $infos(id)] > 8} {
	append infos(version) .1
    }

    namespace export  htmlInfo htmlOption \
	    emit emitVerbatum attrs defaultAttrs \
	    push depth pop isElementOpen \
	    html head title link meta \
	    body h1 h2 h3 h4 h5 h6 p blockquote blockquote* pre script style \
	    ul ol li \
	    dl dt dd \
	    table tr td th td* th* \
	    div map form \
	    hr br area\
	    b i t s u em strong code var dfn cite small \
	    big font span a sup sub \
	    kbd var dfn cite abbr acronym ins del\
    	    textarea\
	    b* i* t* s* u* em* strong* code* var* dfn* cite* \
	    small* big* font* span* a* sub* sub* \
	    kbd* var* dfn* cite* abbr* acronym* ins* del*\
    	    textarea*\
	    img embed object br \
	    amp lt gt quot nbsp \
	    q stylesheet keywords description beginDocument endDocument

    
    # User options
    set opts(syntax) xhtml;	# we have feeble gestures towards XML
    set opts(out) stdout
    set opts(subDir) .;		# where this dir is relative to root
    set opts(rootDir) .;	# where root is relative to this dir
    set opts(htmlRootDir) .;	# root of target for HTML files
    set opts(encoding) UTF-8; 	# encoding (Internet format)

    # Internal state
    set Stack {};		# list of open elements, outermost first
    set Indent {};		# string to insert at the start of lines
}

# htmlInfo --
#  Return misc info for the 
proc html::htmlInfo {key args} {
    variable infos
    if {[llength $args] == 0} {
	return $infos($key)
    } elseif {[llength $args] == 1} {
	set infos($key) [lindex $args end]
    } else {
	error "Too many arguments to htmlInfo"
    }
}

# htmlOption --
#  Set an option controlling the HTML generator.
#  
proc html::htmlOption {key args} {
    variable opts
    if {[llength $args] == 0} {
	return $opts($key)
    } elseif {[llength $args] == 1} {
	set opts($key) [lindex $args end]
	if {[string compare $key subDir] == 0} {
	    foreach d [file split $opts(subDir)] {
		append rd /..
	    }
	    if {[string length $rd] > 1} {
		set rd [string range $rd 1 end]
	    } else {
		set rd .
	    }
	    puts stderr rootDir=$rd
	    set opts(rootDir) $rd
	}
    } else {
	error "Too many arguments to htmlOption"
    }
}

#
#  Primitives for keeping track of open and closed elements.
#

# Log --
#  Emit a message to stderr or similar.
proc Log {text} {
    puts stderr $text
}

# emit --
#  Write the text with the appropriate indentation.
proc html::emit {text} {
    variable opts
    variable Indent
    foreach line  [split $text \n] {
	puts $opts(out) $Indent[string trim $line]
    }
}

# emitVerbatim --
#  Write the text verbatim. Used for PRE, STYLE, and SCRIPT elements.
proc html::emitVerbatim {text} {
    variable opts
    puts $opts(out) $text
}

# push --
#  Open an HTML element TAG with attributres defined by ARGS.
proc html::push {tag args} {
    variable Indent
    variable Stack
    emit "<$tag[attrs $tag $args]>"
    append Indent "  "
    lappend Stack $tag
}

# attrs --
#  Translate a Tcl list of key=value pairs
#  into the format required for XML attributes.
#  Also insert any default attributes for that tag.
# Note --
#  An attribute ~xxx deletes any default xxx=yyy.
# Note --
#  We use the XHTML 1.0 format for attributes,
#  which is supposed to work with HTML and XML parsers.
#  In particular, attrbutes we're used to seeing as single
#  words, (as in <hr noshade>), are written *twice*
#  (as in <hr noshade noshade='noshade'>).  Yuk.
proc html::attrs {tag xs} {
    variable defaults 
    variable opts

    # Append to xs any defaults not already given a value in xs:
    if {[info exists defaults($tag)]} {
	foreach def $defaults($tag) {
	    regexp "^~?(\[^~=]*)" $def dummy key
	    if {[lsearch -regexp $xs "~?${key}(=.*)?$"] < 0} {
		lappend xs $def
    }   }   }

    # Now assemble the attributes in an HTML-friendly syntax:
    set text {}
    foreach x $xs {
	if {[string match ~* $x]} continue

	#### regsub "=(.*\[^a-z0-9.-].*|)\$" $x "=\"\\1\"" x
	##regsub "^(\[^=]+)\$" $x "& &=&" x

	if {"$opts(syntax)" != "html"} {
	    regsub "^(\[^=]+)\$" $x "&=&" x
	}

	# Canonicalize by converting "a = 'b'" to "a=b".
	regsub "\[ \t\r\n]*=\[ \t\r\n]*" $x = x
	regsub "=(\[\"'])(.*)\\1\$" $x "=\\2" x

	# Escape characters special to XML.
	regsub -all -nocase "&amp;" $x "\\&" x
	regsub -all "\\&" $x "\\&amp;" x
	regsub -all "\"" $x "\\&quot;" x
	regsub -all "<" $x "\\&lt;" x
	regsub -all ">" $x "\\&gt;" x

	switch -- $opts(syntax) {
	    html - html/* {
		# Only quote if non-name character included.
		regsub "=(.*\[^a-zA-Z0-9+.-].*)\$" $x "=\"\\1\"" x
	    }
	    default {
		# Quoting is mandatory in XML.
		regsub "=(.*)\$" $x "=\"\\1\"" x
	}   }

	# Check for the deprecated attrribute 'name'.
	if {[regexp -nocase "^name=\"(.*)\"\$" $x dummy val] && \
		[string compare $tag meta] != 0} {
	    switch -- $opts(syntax) {
		html - html/* {
		    # Leave it.
		}
		xhtml - xhtml/* {
		    # Arrange to have id=n appended in addition to name=n.
		    set id $val
		}
		xml - xml/* {
		    # Replace name=n with id=n.
		    set x "id=\"$val\""
	    }   }   
	} elseif {[string match id=* $x]} {
	    set hadId 1
	}

	append text " " $x
    }

    if {[info exists id] && ![info exists hadId]} {
	append text " id=\"$id\""
    }

    return $text
}

# defaultAttrs --
#  Set or return the default attrbiutes for TAG.
proc html::defaultAttrs {tag args} {
    variable defaults
    if {[llength $args] == 0} {
	if {[info exists defaults($tag)]} {
	    return $defaults($tag)
	} {
	    return {}
	}
    } else {
	set defaults($tag) $args
}   }

# depth --
#  Return the number of open elements (= the depth of the element stack).
proc html::depth {} {
    variable Stack
    return [llength $Stack]
}

# pop --
#  Close the most recent element opened with `push'.
#  Returns the tag removed from the stack.
proc html::pop {} {
    variable Stack
    variable Indent
    set tag [lindex $Stack end]
    set Stack [lrange $Stack 0 [expr [depth] - 2]]
    set Indent [string range $Indent 2 end]
    emit </$tag>
    return $tag
}

# isElementOpen --
#  Return nonzero iff the named element is open,
#  i.e., is to be foind on the stack of open elements.
proc html::isElementOpen {tag} {
    variable Stack
    return [expr [lsearch -exact $Stack $tag] >= 0]
}

# BlockElement1 --
#  Generic implementation for block elements.
#  All the arguments but the last are attrbiutes
#  for the newly open element.  The final arg
#  is a Tcl script for generating the contents of the element.
proc html::BlockElement1 {tag args} {
    if {[llength $args] > 0} {
	set script [lindex $args end]
	set args [lrange $args 0 [expr [llength $args] - 2]]
	eval push $tag $args
	uplevel $script
	pop
    } else {
	emit "<$tag></$tag>"
    }
}

# BlockElement --
#  Generic implementation for block elements.
#  Same as BlockElement1, except that it checks that
#  the PARENT element is already open.
#  All the arguments but the last are attrbiutes
#  for the newly open element.  The final arg
#  is a Tcl script for generating the contents of the element.
proc html::BlockElement {tag parent args} {
    variable opts
    if {![isElementOpen $parent]} {
	error "`$tag' elements must go within `$parent'"
    }
    if {[llength $args] > 0} {
	set script [lindex $args end]
	set args [lrange $args 0 [expr [llength $args] - 2]]
	eval push $tag $args
	uplevel $script
	pop
    } else {
	emit "<$tag></$tag>"
}   }

# ParElement1 --
#  Generic implementatiom for element that take text as their content.
#  The last argument is the text to use as the element content.
proc html::ParElement1 {tag args} {
    variable opts
    if {[llength $args] > 0} {
	set text [string trim [lindex $args end]]
	set args [lrange $args 0 [expr [llength $args] - 2]]
	if {[string first \n $text] >= 0} {
	    eval push $tag $args
	    emit $text
	    pop
	} else {
	    emit "<$tag[attrs $tag $args]>$text</$tag>"
	}
    } else {
	emit "<$tag></$tag>"
}   }

# ParElement --
#  Like ParElement1, except that it checks that a PARENT element is open.
proc html::ParElement {tag parent args} {
    variable opts
    if {![isElementOpen $parent]} {
	error "`$tag' belongs inside `$parent'"
    }
    if {[llength $args] > 0} {
	set text [string trim [lindex $args end]]
	set args [lrange $args 0 [expr [llength $args] - 2]]
	if {[string first \n $text] >= 0} {
	    eval push $tag $args
	    emit $text
	    pop
	} else {
	    emit "<$tag[attrs $tag $args]>$text</$tag>"
	}
    } else {
	emit <$tag></$tag>
}   }

# EmptyElement1 --
#  Generic implementation for element that take no content.
# Note --
#  We use the XHTML 1.0 syntax, which should work with
#  both HTML and XML parsers.
proc html::EmptyElement1 {tag args} {
    variable opts
    switch $opts(syntax) {
	html {
	    emit "<$tag[attrs $tag $args]>"
	}
	xml {
	    emit "<$tag[attrs $tag $args]/>"
	}
	default {
	    emit "<$tag[attrs $tag $args] />"
	}
    }
}

# EmptyElement --
#  Like EmptyElement1, except that it checks that a PARENT element is open.
proc html::EmptyElement {tag parent args} {
    if {![isElementOpen $parent]} {
	error "`$tag' belongs inside `$parent'"
    }
    eval EmptyElement1 $tag $args
}

# InlineTextElement --
#  A generic implementation of in-line text elements.
#  These return an HTML value to be subtituted into
#  the text of a TextElement like `p' or `h1'.
#  All args but the last define attributes of the element.
#  The final arg is the content of the element.
proc InlineTextElement {tag args} {
    set text [lindex $args end]
    set args [lrange $args 0 [expr [llength $args] - 2]]
    return "<$tag[attrs $tag $args]>$text</$tag>"
}

# InlineEmptyElement --
#  A generic implementation of in-line text elements.
#  These return an HTML value to be subtituted into
#  the text of a TextElement like `p' or `h1'.
#  The args define attributes of the element.
# Note --
#  We use the XHTML 1.0 syntax, which should work with
#  both HTML and XML parsers.
proc html::InlineEmptyElement {tag args} {
    return "<$tag[attrs $tag $args] />"
}


#
#  What follows is in effect our DTD, encoded in the form of Tcl procs...
#

proc html::html {args} {
    uplevel html::BlockElement1 html $args
}
foreach tag {head body} {
    proc html::$tag {args} "
        uplevel html::BlockElement $tag html \$args
    "
}
proc html::title {args} {
    eval ParElement title head $args
}
proc html::link {href args} {
    eval EmptyElement link head [list href=$href] $args
}
proc html::base {href args} {
    eval EmptyElement base head [list href=$href] $args
}
proc html::meta {args} {
    eval EmptyElement meta head $args
}

foreach tag {h1 h2 h3 h4 h5 h6 p blockquote} {
    proc html::$tag {args} "
        eval html::ParElement $tag body \$args
    "
}
foreach tag {ul ol dl table div form ins* del*} {
    proc html::$tag {args} "
        uplevel html::BlockElement $tag body \$args
    "
}

proc html::blockquote* {args} {
    uplevel html::BlockElement blockquote body $args
}

proc html::map {args} {
    uplevel html::BlockElement1 map $args
}
proc html::area {href coords args} {
    uplevel html::EmptyElement area map [list href=$href coords=$coords] $args
}

# pre --
#  This is trickier because we want to preserve all spacing,
#  so can't use any of the generic implementations above
#  (since they all munge the indentation).
#  Also, we accept an optional argument -encode
#  which means to map the characters special to SGML (&, <, and >)
#  to their entity representation.  This is must useful when
#  we want to quote a source file, say, verbatim.
proc html::pre {args} {
    set encode 0
    set scan 1
    while {$scan} {
	switch -exact -- [lindex $args 0] {
	    -encode {
		set encode 1
		set args [lrange $args 1 end]
	    }
	    -- {
		set args [lrange $args 1 end]
		set scan 0
	    }
	    default {
		set scan 0
    }   }   }
    set text [lindex $args end]
    set args [lrange $args 0 [expr [llength $args] - 2]]
    if {$encode} {
	regsub -all & $text "\\&amp;" text
	regsub -all < $text "\\&lt;" text
	regsub -all > $text "\\&gt;" text
    }
    emit <pre[attrs pre $args]>
    emitVerbatim $text</pre>
}

# SCRIPT, STYLE --
# In XML files, use CDATA; in HTML, use cheezy comments.
# In XHTML, I'm not sure what we should do...!
#
# Note! To make an empty script element, you must 
# have an emptry string as last arg:
#  script src=foo.js {}
proc html::scriptOrStyle {tag args} {
    variable opts

    set text [lindex $args end]
    set args [lrange $args 0 [expr [llength $args] - 2]]

    switch $opts(syntax) {
	html/4 - html {
	    set text "<!--\n$text\n// -->" 
	}
	xml {
	    set text "<!\[CDATA\[\n$text\n\]\]>"
	}
    }
    eval push $tag $args
    emitVerbatim $text
    pop
}

proc html::script {args} {
    eval html::scriptOrStyle script $args
}

proc html::style {args} {
    if {![isElementOpen head]} {
	error "`style' elements must go within `head'"
    }
    eval html::scriptOrStyle style $args
}


proc html::li {args} {
    if {![isElementOpen ol] && ![isElementOpen ul]} {
	error "`li' must go inside `ol' or `ul'"
    }
    eval ParElement1 li $args
}

# dl {
#  dt TEXT
#  dd TEXT
# }
foreach tag {dt dd} { 
    proc html::$tag {args} "
	eval ParElement $tag dl \$args
    "
}

foreach tag {caption colgroup thead tfoot tbody tr} {
    proc html::$tag {args} "uplevel html::BlockElement $tag table \$args"
}
proc html::td* {args} {
    uplevel html::BlockElement td tr $args
}
proc html::th* {args} {
    uplevel html::BlockElement th tr $args
}
proc html::td {args} {
    eval ParElement td tr $args
}
proc html::th {args} {
    eval ParElement th tr $args
}
proc html::col {args} {
    eval EmptyElement col table $args
}

foreach tag {hr br} {
    proc html::$tag {args} "
        eval EmptyElement $tag body \$args
    "
}

proc html::br* {args} {
    eval EmptyElement br body $args
}


foreach tag {
    b i s u em code tt strong cite small big font span a sup sub
    kbd var dfn cite abbr acronym ins del
    textarea
} {
    proc html::$tag* {args} "
    	return \[eval InlineTextElement $tag \$args]
    "
    proc html::$tag {args} "
    return \[eval InlineTextElement $tag \[list \[join \$args { }]]]
    "
}
proc html::a {href args} {
    return [eval InlineTextElement a [list href=$href] [list [join $args " "]]]
}
foreach tag {br input} {
    proc html::$tag {args} "
    	return \[eval InlineEmptyElement $tag \$args]
    "
}

# img --
#  An in-line image tag.
#  Examines the image file, if possible, to deduce the default 
#  width and height of the image.
proc html::img {src args} {
    variable opts
    variable widths
    variable heights
    variable nImages
    variable defaults

    # do we need to supply a width or height?
    array set needs {
	width 0
	height 0
    }
    foreach key {width height} {
	if {[lsearch -regexp $args "~?${key}(=.*)?$"] < 0 \
		&& (![info exists defaults(img)] \
		|| [lsearch -regexp $defaults(img) "~?${key}(=.*)?$"] < 0)} {
	    set needs($key) 1
    }   }

    if {$needs(width) || $needs(height)} {
	# have we read in the data file yet?
	if {![info exists widths] && [file exists img.data]} {
	    Log "Reading img.data"
	    interp create -safe tmpInterp
	    tmpInterp alias img ::html::Img
	    set nImages 0
	    interp invokehidden tmpInterp source img.data
	    interp delete tmpInterp
	    Log "Read $nImages img infos."
	}

	if {![info exists widths($src)]} {
	    foreach dir [list $opts(htmlRootDir) $opts(rootDir)] {
		set imgFile [file nativename \
			[file join $dir $opts(subDir) $src]]
		if {[file exists $imgFile]} {
		    break
	    }   }
	    Log "Examining $imgFile:"
	    
	    switch -glob -- $src {
		*.jpg - *.jpeg - *.JPG {
		    set cmd [list exec djpeg $imgFile] 
		}
		*.gif - *.GIF {
		    set cmd [list exec giftopnm $imgFile]
		}
		*.png - *.PNG {
		    set cmd [list exec pngtopnm $imgFile]
		}
		default {
		    set cmd [list exec anytopnm $imgFile]
	    }   }
	    set filt [list 2>@stderr | sh -c {pnmfile 2>&1; cat >/dev/null}]
	    if {[catch [concat $cmd $filt] res]} {
		error "command '$cmd' failed '$res'"
	    }
	    set widths($src) [lindex $res 3]
	    set heights($src) [lindex $res 5]
	    Log "\t- size=$widths($src)x$heights($src)"
	    
	    set out [open img.data a]
	    puts $out [list img $src $widths($src) $heights($src)]
	    close $out
	}

	if {$needs(width)} {
	    lappend args width=$widths($src)
	}
	if {$needs(height)} {
	    lappend args height=$heights($src)
	}
    }
    return [eval InlineEmptyElement img [list src=$src] $args]
}

proc html::Img {src width height} {
    variable widths
    variable heights
    variable nImages

    set widths($src) $width
    set heights($src) $height
    incr nImages
}

proc html::embed {src args} {
    # TODO. Look for type attribute
    return [eval InlineEmptyElement embed [list src=$src] $args]
}

# object --
#  This is an INLINE element (like 'img').
#  It needs to have both attributes and parameters
#  as arguments, AND the content to use if the object cannot be shown.
#  So the syntax is as follows:
# object foo=bar bar=baz [ -params foo=bar baz=quux ] content
proc html::object {args} {
    # content is last arg.
    set content [lindex $args end]
    set args [lrange $args 0 [expr [llength $args] - 2]]

    set pos [lsearch -exact $args -params]
    if {$pos == 0} {
	set params [lrange $args 1 end]
	set args {}
    } elseif {$pos == [llength $args] - 1} {
	set args [lrange $args [expr [llength $args] - 2]]
	set params {}
    } elseif {$pos > 0} {
	set params [lrange $args [expr $pos + 1] end]
	set args [lrange $args [expr $pos - 1]]
    } else {
	set params {}
    }

    set result "<object[attrs object $args]>"
    foreach param $params {
	regexp (.*)=(.*) $param dummy key val
	append result "\n  <param name=\"$key\" value=\"$val\" />"
    }
    return "$result\n$content\n</object>"
}

foreach entity {amp lt gt quot nbsp} {
    proc html::$entity {} "return {&$entity;}"
}



#
# "Derrived" elements -- these evaluate into 
#  one or more HTML elements
#

proc html::q {args} {
    return "[quot][join $args " "][quot]"
}

# stylesheet --
#  Generate a link to an "external" style sheet (.css file).
#  This must go in the head of a document.
proc html::stylesheet {href args} {
    eval link $href rel=stylesheet type=text/css $args
}

# keywords --
#  Generate a META tage containing keywords for search engines.
proc html::keywords {args} {
    meta name=Keywords content=[join $args " "]
}

# description --
#  Generate a META tage containing keywords for search engines.
proc html::description {text} {
    meta name=Description content=$text
}

proc html::svg {uri args} {
    return [eval InlineEmptyElement embed [list src=$uri type=image/svg+xml]\
	    $args]
}



# beginDocument --
#  Start writing an HTML document.
#  This creates or replaces the output file, emits the
#  DOCTYPE element, and 
#  opens the HTML element that encompasses everything,
#  generates the HEAD element, and opens the BODY element.
# Arguments --
#  outFileName -- the name of the file to create
#  args -- all except the last optional argument are
#    attributes for the BODY element. The final arg is a script
#    which is evaluated to generate the contents of the HEAD element.

proc html::beginDocumentHelper {rootEType headEType bodyEType args} {
    global argv0 argv
    variable opts
    variable infos

    set outFileName [file rootname $infos(inFileName)].html
    set scan 1
    while {$scan} {
	switch -exact -- [lindex $args 0] {
	    -file {
		set outFileName [lindex $args 1]
		set args [lrange $args 2 end]
	    }
	    -encoding {
		set opts(encoding) [lindex $args 1]
		set args [lrange $args 2 end]
	    }
	    -- {
		set args [lrange $args 1 end]
		set scan 0
	    }
	    default {
		set scan 0
    }   }   }

    set opts(out) [open $outFileName w]
    if {[info tclversion] > 8.1} {
	regsub iso- [string tolower $opts(encoding)] iso e
	fconfigure $opts(out) -encoding $e
    }
    set infos(outFileName) $outFileName

    # Write the output file name to foo.files.
    if {![info exists infos(filesFile)]} {
	set filesFileName [file root $infos(inFileName)].files
	set infos(filesFile) [open $filesFileName w]
	set infos(filesFileName) $filesFileName
    }
    puts $infos(filesFile) $outFileName
    flush $infos(filesFile)

    set script [lindex $args end]
    set args [lrange $args 0 [expr [llength $args] - 2]]
    switch $opts(syntax) {
	html/4 - html {
	    emit "<!DOCTYPE HTML\
		    PUBLIC '-//W3C//DTD HTML 4.0 Transitional//EN'\n\t\
		    'http://www.w3.org/TR/REC-html40/loose.dtd'>"
	    push html
	}
	xhtml/1 - xhtml {
	    emit "<!DOCTYPE html\
		    PUBLIC '-//W3C//DTD XHTML 1.0 Transitional//EN'\n\t\
		    'http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd'>"
	    push html xmlns=http://www.w3.org/1999/xhtml
	}
	xml {
	    emit "<?xml version='1.0' encoding='$opts(encoding)'?>"
	    emit "<!DOCTYPE html\
		    PUBLIC '-//W3C//DTD XHTML 1.0 Transitional//EN'\n\t\
		    'http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd'>"
	    push html xmlns=http://www.w3.org/1999/xhtml
	}
	default {
	    push html
	}
    }
    emit "<!-- Generated from [htmlInfo inFileName] on\
	    [clock format [clock seconds] -format "%Y-%m-%d %H:%M %Z"] -->"
    regsub -all -- --subdir $argv -s xs
    regsub -all -- --html_?root $xs -r xs
    regsub -all -- -- $xs "- -" xs
    emit "<!-- with command: cd [pwd];"
    emit "\t$argv0 [join $xs " "] -->"
    emit "<!-- htmlProcs.tcl version [htmlInfo version] -->"
    push head
    uplevel $script
    meta http-equiv=Content-type "content=text/html; charset=$opts(encoding)"
    pop

    if {[string compare $bodyEType ""] != 0} {
	eval push $bodyEType $args
    }
}

proc html::beginDocument {args} {
    uplevel beginDocumentHelper html head body $args
}


# endDocument --
#  Finish the documnent started with beginDocument.
proc html::endDocument {} {
    variable opts
    variable infos

    if {[depth] > 2} {
	error "`endDocument' belongs after all page content"
    }
    pop; # body
    pop; # html
    close $opts(out)
    set opts(out) stdout
    set infos(outFileName) {}
}

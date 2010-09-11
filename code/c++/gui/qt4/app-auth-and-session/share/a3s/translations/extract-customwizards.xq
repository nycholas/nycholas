let $files := ( "" )
let $prefix := string("QT_TRANSLATE_NOOP(&quot;ProjectExplorer::CustomWizard&quot;, &quot;")
let $suffix := concat("&quot;)", codepoints-to-string(10))
where empty($files)
return
for $file in $files
    let $doc := doc($file)
    for $text in ($doc/*:wizard/*:description, $doc/*:wizard/*:displayname, $doc/*:wizard/*:displaycategory, $doc/*:wizard/*:fieldpagetitle, $doc/*:wizard/*:fields/*:field/*:fielddescription)
        return fn:concat($prefix, data($text), $suffix)

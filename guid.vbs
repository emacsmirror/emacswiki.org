<pre>
'Adds a link to the currently selected message to the clipboard
Set Outlook = createobject("Outlook.Application")
Set objMail = Outlook.ActiveExplorer.Selection.Item(1)
Wscript.echo ("[[outlook:" + objMail.EntryID + "][MESSAGE: " + objMail.Subject +" ("+ objMail.SenderName +")]]")
</pre>

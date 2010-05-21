<pre>
Set Outlook = createobject("Outlook.Application")
If Outlook.Application.ActiveExplorer.Selection.Count > 0 Then
        For I = 1 To Outlook.Application.ActiveExplorer.Selection.Count
                Set objMail = Outlook.ActiveExplorer.Selection.Item(I)
                Wscript.echo ("** TODO " + objMail.Subject + " :OFFICE:")
                Wscript.echo ("[[outlook:" + objMail.EntryID + "][MESSAGE: " + objMail.Subject +" ("+ objMail.SenderName +")]]")
                Wscript.echo ""
                Wscript.echo objMail.Body
                Wscript.echo ""
                Wscript.echo ""
        Next
End If
</pre>

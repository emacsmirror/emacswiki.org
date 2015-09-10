hello,there is a bug:
{{{
<?xml version="1.0" encoding="UTF-8" standalone="no"?>                                                    
<?fileVersion 4.0.0?><cproject storage_type_id="org.eclipse.cdt.core.XmlProjectDescriptionStorage">       
    <storageModule moduleId="org.eclipse.cdt.core.settings">                                              
        <cconfiguration id="com.android.toolchain.gcc.1892026188">                                        
            <storageModule buildSystemId="org.eclipse.cdt.managedbuilder.core.configurationDataProvider" id="com.android.toolchain.gcc.1892026188" moduleId="org.eclipse.cdt.core.settings" name="Default">        
                <externalSettings/>                                                                       
                <extensions>                                                                              
                    <extension id="org.eclipse.cdt.core.ELF" point="org.eclipse.cdt.core.BinaryParser"/>  
                    <extension id="org.eclipse.cdt.core.VCErrorParser" point="org.eclipse.cdt.core.ErrorP…
-UUU(DOS)----F1  .cproject      Top (1,0)      (nXML Valid Fill) 四  9 10 15:43 3.52 ---------------------
Error in post-command-hook (col-highlight-highlight): (wrong-type-argument characterp -1)
}}}

-- 匿名者 2015-09-10 07:43 UTC


----

Sorry, but you will need to provide more info than that. A reproducible recipe, for example.  And whether option `col-highlight-vline-face-flag' is nil or non-nil.

The bug, if there is one, is presumably in `vline-show' of library [[vline.el]], since `col-highlight-highlight' just calls that function. Consider contacting the maintainer of ##vline.el## -- but be prepared to provide more info.

-- DrewAdams 2015-09-10 13:49 UTC


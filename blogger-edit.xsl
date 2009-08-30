<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@google.com>
License: GPL
Copyright: This file is part of the atom-blogger package.
Filters out elements that should not appear in edits.
Adds generator element.
-->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:atom="http://purl.org/atom/ns#"
  xmlns="http://purl.org/atom/ns#">
  <xsl:output method="xml"  indent="yes"/>
  
<xsl:template match="atom:id|atom:issued|atom:modified|atom:created"/>  
  <xsl:template match="*|@*" >
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>

  </xsl:template>
  <xsl:template match="atom:entry">
    <entry>
<generator url="http://purl.org/net/emacs-atom-blogger/">Emacs Atom Blogger </generator>
  <xsl:apply-templates/>
    </entry>
  </xsl:template>
</xsl:stylesheet>

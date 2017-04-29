<?xml version="1.0" ?>
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!--
***** Containers *****
-->

<!-- pages -->
<xsl:template match="page">
    <html>
        <body>
            <h1><xsl:value-of select="@title"/></h1>
            <xsl:apply-templates select="*"/>
            </body>
    </html>
</xsl:template>

<!-- sections -->
<xsl:template match="section">
    <h2><xsl:value-of select="@title"/></h2>
    <xsl:apply-templates select="*"/>
</xsl:template>

<!-- subsections -->
<xsl:template match="subsection">
    <h3><xsl:value-of select="@title"/></h3>
    <xsl:apply-templates select="*"/>
</xsl:template>

<!-- subsubsections -->
<xsl:template match="subsubsection">
    <h4><xsl:value-of select="@title"/></h4>
    <xsl:apply-templates select="*"/>
</xsl:template>

<!-- paragraphs -->
<xsl:template match="para">
    <p><xsl:apply-templates select="*"/> </p>
</xsl:template>

<!--
***** Leaf nodes *****
-->

<!-- text -->
<xsl:template match="text">
    <xsl:value-of select="text()"/>
</xsl:template>

<!-- links -->
<xsl:template match="link">
    <xsl:element name="a">
        <xsl:attribute name="href"><xsl:value-of select="@href"/></xsl:attribute>
        <xsl:value-of select="text()"/>
    </xsl:element>
</xsl:template>

<!-- images -->
<xsl:template match="img">
    <xsl:element name="img">
        <xsl:attribute name="src"><xsl:value-of select="@src"/></xsl:attribute>
    </xsl:element>
</xsl:template>

</xsl:transform>


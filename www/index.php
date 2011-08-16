
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

<p> For a <strong>detailed description</strong> of the package see the <a href="http://cran.r-project.org/web/packages/QuACN/vignettes/QuACN.pdf"><strong>vignette</strong></a>. </p>

<p> <strong>General Information</strong> about QuACN:</p>
<ol>
	<li>Installation</li>
	To install QuACN following software has to be installed before:
	<ul><li><a href="GMP" target="http://gmplib.org/">GMP</a></li></ul>
   <ul><li><a href="MPFR" target="http://www.mpfr.org/	">MPFR</a></li></ul>
<!-- 	<li>Available Descriptors</li> -->
	<li>Publications</li>
	<ul><li>Mueller LAJ, Kugler KG, Dander A, Graber A, Dehmer M (2011) <em>QuACN: an R package for analyzing complex biological networks quantitatively.</em> Bioinformatics 27: 140–141.</li></ul>
	<ul><li>Mueller LAJ, Kugler KG, Dehmer M (2011) <em>Stuctural Analysis of Molecular Networks : AMES Mutagenicity.</em>BIOCOMP, Las Vegas</li></ul>
   <ul><li>Kugler KG, Mueller LAJ, Dehmer M (2011) <em>Integrative Network Biology: Graph Prototyping for Co-Expression Cancer Networks.</em> PLoS ONE 6(7): e22843. doi:10.1371/journal.pone.0022843</li></ul>
</ol>

<ul></ul>



</body>
</html>

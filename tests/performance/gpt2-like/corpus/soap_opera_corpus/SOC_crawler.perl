
use LWP::Simple qw($ua get);
use HTTP::Cookies;
use  HTTP::Headers ;
use URI;

use HTML::Entities;
my %seen=();

$ua->timeout(30);
$cookie="adultOff=erotica; smashaffiliatetoken=FCPIPWKHIVGPPOBW";
#my $cookies = HTTP::Cookies->new();
#$cookies->set_cookie(0,'adultOff', 'erotica','/','smashwords.com',80,0,0,86400,0);
#$cookies->set_cookie(0,'smashaffiliatetoken', 'FCPIPWKHIVGPPOBW','/','smashwords.com',80,0,0,86400,0);
#$ua->cookie_jar($cookies);
 $h = HTTP::Headers->new;
 #$h->header('Cookie' => $cookie);
 $ua->default_headers($h);
 $corpusFile = "soc_corpus.txt";
 preloadCache();
 
 open(FBOOK, ">$corpusFile");
  my $sleepDelay = 1;
  my $debug=0;
  
  #fetchTranscript("charmed",'http://tvmegasite.net/prime/shows/charmed/transcripts/season5/ep5-20.shtml',1);
  #fetchTranscript("charmed",'http://tvmegasite.net/prime/shows/charmed/transcripts/season5/ep5-20.shtml',1);
  #exit;
  
  
  fetchAllMyChildren();
  fetchAsTheWorldTurns();
  fetchBoldAndBeautiful();
  fetchAnotherWorld();
  fetchGeneralHospital();
  fetchGuidingLight();
  fetchOneLifeToLive();
  fetchPassions();
  fetchPortCharles();
  fetchYoungAndRestless();
  
  fetchPrimeTime();
  exit;

sub fetchAllMyChildren
{
	
	for ($year=1998;$year<=2013;$year++)
	{
		fetchYearIndex("All My Children",'http://tvmegasite.net/transcripts/amc/main/'. $year .'transcripts.shtml');
	}
}

sub fetchAsTheWorldTurns
{
	for ($year=2001;$year<=2010;$year++)
	{
		fetchYearIndex("As The World Turns",'http://tvmegasite.net/transcripts/atwt/main/'. $year .'transcripts.shtml');
	}
}
sub fetchBoldAndBeautiful
{
	for ($year=2001;$year<=2010;$year++)
	{
		fetchYearIndex("Bold And Beautiful",'http://tvmegasite.net/transcripts/bb/main/'. $year .'transcripts.shtml');
	}
}

sub fetchAnotherWorld
{
	for ($year=2003;$year<=2007;$year++)
	{
		fetchYearIndex("Another World",'http://tvmegasite.net/transcripts/aw/main/'. $year .'transcripts.shtml');
	}
}
sub fetchAnotherWorld
{
	for ($year=2001;$year<=2017;$year++)
	{
		fetchYearIndex("Days of Our Lives",'http://tvmegasite.net/transcripts/days/main/'. $year .'transcripts.shtml');
	}
}
sub fetchGeneralHospital
{
	fetchYearIndex("General Hospital",'http://tvmegasite.net/transcripts/gh/main/1963-97transcripts.shtml');
	for ($year=1998;$year<=2017;$year++)
	{
		fetchYearIndex("General Hospital",'http://tvmegasite.net/transcripts/gh/main/'. $year .'transcripts.shtml');
	}
}
sub fetchGuidingLight
{

	for ($year=2001;$year<=2009;$year++)
	{
		fetchYearIndex("Guiding Light",'http://tvmegasite.net/transcripts/gl/main/'. $year .'transcripts.shtml');
	}
}
sub fetchOneLifeToLive
{

	for ($year=1998;$year<=2013;$year++)
	{
		fetchYearIndex("One Life To Live",'http://tvmegasite.net/transcripts/oltl/main/'. $year .'transcripts.shtml');
	}
}
sub fetchPassions
{

	for ($year=1999;$year<=2008;$year++)
	{
		fetchYearIndex("Passions",'http://tvmegasite.net/transcripts/passions/main/'. $year .'transcripts.shtml');
	}
}
sub fetchPortCharles
{

		fetchYearIndex("Port Charles",'http://tvmegasite.net/day/pc/oldtranscripts.shtml');

}
sub fetchYoungAndRestless
{

	for ($year=2001;$year<=2017;$year++)
	{
		fetchYearIndex("The Young and The Restless",'http://tvmegasite.net/transcripts/yr/main/'. $year .'transcripts.shtml');
	}
}

sub fetchPrimeTime
{

	fetchPrimeIndex("Buffy",'http://tvmegasite.net/prime/shows/buffy/transcripts/season1.shtml');
	fetchPrimeIndex("Buffy",'http://tvmegasite.net/prime/shows/buffy/transcripts/season2.shtml');
	fetchPrimeIndex("Buffy",'http://tvmegasite.net/prime/shows/buffy/transcripts/season3.shtml');
	fetchPrimeIndex("Buffy",'http://tvmegasite.net/prime/shows/buffy/transcripts/season4.shtml');
	fetchPrimeIndex("Buffy",'http://tvmegasite.net/prime/shows/buffy/transcripts/season5.shtml');
	fetchPrimeIndex("Buffy",'http://tvmegasite.net/prime/shows/buffy/transcripts/season6.shtml');
	fetchPrimeIndex("Buffy",'http://tvmegasite.net/prime/shows/buffy/transcripts/season7.shtml');
	
	fetchPrimeIndex("Boston Legal",'http://tvmegasite.net/prime/shows/boston/transcripts.shtml');
	fetchPrimeIndex("Smallville ",'http://tvmegasite.net/prime/shows/smallville/transcripts.shtml');
	fetchPrimeIndex("Charmed",'http://tvmegasite.net/prime/shows/charmed/transcripts.shtml');
	#fetchPrimeIndex("The O.C.",'http://tvmegasite.net/prime/shows/oc/transcripts.shtml');
	#fetchPrimeIndex("Lost",'http://tvmegasite.net/prime/shows/lost/transcripts.shtml');
	

}
sub fetchYearIndex
{

	my $series = $_[0];
	my $pageLink = $_[1];
	
	print "YEARINDEX\t$series\t$pageLink\n";
	print FBOOK "YEARINDEX\t$series\t$pageLink\n";

	# sourc page http://tvmegasite.net/transcripts/amc/main/2001transcripts.shtml
	# <a href="../older/2001/amc-trans-04-09-01.shtml">4/9/01</a> 
	# want http://tvmegasite.net/transcripts/amc/older/2001/amc-trans-10-22-01.shtml
	
	my $pageURL = $pageLink;
	my $pageContent = get($pageURL);


	@links = $pageContent =~ /\<a href\=\"(.*?)\"\>[A-Za-z0-9\/ \.\n\r\t]+\<\/a\>/gi;
	$baseURL = $pageLink;
	foreach my $link(@links)
	{
		$new_abs_link = URI->new_abs($link, $baseURL)->canonical;
		if ($new_abs_link !~ /htm/) {next;}
		if ($new_abs_link !~ /[0-9]\-/) {next;}
		if ($new_abs_link !~ /tvmegasite\.net\/transcripts/) {next;}
		print "TLINK\t$series\t$new_abs_link\n";
		print FBOOK "TLINK\t$series\t$new_abs_link\n";
		fetchTranscript ($series,$new_abs_link);
	}
	sleep($sleepDelay);
}	

sub fetchPrimeIndex
{

	my $series = $_[0];
	my $pageLink = $_[1];
	
	print "PYEARINDEX\t$series\t$pageLink\n";
	print FBOOK "PYEARINDEX\t$series\t$pageLink\n";

	# sourc page http://tvmegasite.net/transcripts/amc/main/2001transcripts.shtml
	# <a href="../older/2001/amc-trans-04-09-01.shtml">4/9/01</a> 
	# want http://tvmegasite.net/transcripts/amc/older/2001/amc-trans-10-22-01.shtml
	
	my $pageURL = $pageLink;
	my $pageContent = get($pageURL);


	@links = $pageContent =~ /\<a href\=\"(.*?)\"\>[A-Za-z0-9\/ \.\n\r\t\&\;\:\#\-\,\']+\<\/a\>/gi;
	$baseURL = $pageLink;
	foreach my $link(@links)
	{
		$new_abs_link = URI->new_abs($link, $baseURL)->canonical;
		# print "RLINK\t$series\tbaseURL\t$link\t$new_abs_link\n";
		# print FBOOK "RLINK\t$series\tbaseURL\t$link\t$new_abs_link\n";

		if ($new_abs_link !~ /htm/) { next;}
		if (($new_abs_link !~ /(oc|lost|boston)/) && ($new_abs_link !~ /[0-9]\-/)) {next;}
		if ($new_abs_link !~ /tvmegasite\.net\/prime.*transcripts/) { next;}
		if ($new_abs_link =~ /tvmegasite\.net\/prime\/shows\/(oc|lost|boston)\/transcripts\.shtml/) {next;}
		
		print "TLINK\t$series\t$new_abs_link\n";
		print FBOOK "TLINK\t$series\t$new_abs_link\n";
		$pageFormat=1;
		if ($series =~ /smallville/gi) {$pageFormat=0;}
		if ($series =~ /boston/gi) {$pageFormat=0;}
		fetchTranscript ($series,$new_abs_link,1);
	}
	sleep($sleepDelay);
}

sub fetchTranscript
{
	my $series = $_[0];
	my $pageLink = $_[1];
    my $pageFormat =$_[2] || 0;
	
	my $pageURL = $pageLink;
	my $pid = $pageLink =~ /\/([a-zA-Z0-9\-]+)\.shtml/;
	if (exists $seen{$pageURL}){ print "SKIP $pageURL\n"; return;}
	# print "TRY:$pageURL\n";
	my $pageContent = get($pageURL);
	# print "GOT:$pageURL\n";
	$pageContent =~ s/(\n|\r|\t)/ /gi;
	$pageContent =~ s/\<\!\-\-msthemeseparator\-\-\>/\<p\>[SCENECHANGE]\<\/p\>/gi;
	$pageContent =~ s/[*][&]nbsp[;][&]nbsp[;] [*][&]nbsp[;][&]nbsp[;] [*][&]nbsp[;][&]nbsp[;]/\<p\>[SCENECHANGE]\<\/p\>/gi;
	# $pageContent =~ s/\<\!\-\-msthemeseparator\-\-\>\<p align\=\"center\"\>//gi;
	$pageContent =~ s/<p (align|class|style)\=\".*?\"\>/\<p\>/gi;
	$pageContent =~ s/\<br style.*\>/\<br\>/gi;
	
	my ($body) = $pageContent =~ /\<body .*?"\>(.*?)\<\/body/;
	my ($title) = $pageContent =~ /\<title\>(.*?)\<\/title\>/;
	$body =~ s/\s+/ /g; 
	$body =~ s/\t/ /g; 
	$body =~ s/\n/ /g; 
	$body =~ s/\r/ /g; 
	$body =~ s/&amp;/&/g; 
	$body =~ s/&nbsp;/ /g; 
	$body =~ s/&quot;/\"/g;	
	
	$title =~ s/&nbsp;/ /g; 
	$title =~ s/&quot;/\"/g;	
	
	while ($body =~ /  /) {$body=~ s/  / /gi;}
	#$body =~ s/<br .*?>/    /gi; 
	#$body =~ s/<.*?>/ /gi; 
	my @lines =();
	#if ($pageContent =~ /\<p\>[A-Za-z]+\:\&nbsp\;(.*?)\<\/p\>/) {$pageFormat =0;}
	if ($pageFormat ==0)
	{
		print FBOOK "REG-FORMAT\n";
		$body =~ s/\<p\>/<\/p>\<p\>/gi;
		@lines = $body =~ /\<p(.*?)\<\/p\>/gi;
	
	}
	else
	{
		print FBOOK "DECODE-FORMAT\n";
		if ( $pageContent =~ /\<p\>([^\\])+\<p\>/ )
		{
			$pageContent =~ s/\<p\>/<\/p>\<p\>/gi;
		}
		$pageContent =~ s/\<o:p\>//gi;
		$pageContent =~ s/\<\/o:p\>//gi;

		@text = $pageContent =~ /\<p\>(.*?)\<\/p\>/gi;
		@lines =();
		foreach $line (@text)
		{
			if ($line =~ /\<p\>/)
			{
				@frags = split(/\<p\>/,$line);
				foreach $f (@frags)
				{
					$f =~ s/<.*?>/ /gi;

					if ($f =~ /\:/)
					{
						$f=fix_chars_in_document($f);
						$f =~ s/^ //;
						push @lines,">$f";
					    # print FBOOK "YFRAG\t$f\n";
					}
				}
			}
			else
			{
			    if ($debug==1) {print FBOOK "PFRAG\t$line\n";}
				if ($line =~ /\<br\>/)
				{
					@frags = split(/\<br\>/,$line);
					foreach $f (@frags)
					{
						$f =~ s/<img (.*?)\>/\<img\>/gi;
						if ($f =~ /\:/)
						{
							$f=fix_chars_in_document($f);
							$f =~ s/^ //;
							push @lines,">$f";
						    if ($debug==1) {print FBOOK "FRAG\t$f\n";}
						}
					}
				}
				else
				{
					$line =~ s/<.*?>/ /gi;
					if (($line =~ /^[A-Za-z ]+\:\&nbsp\;(.*?)/) || ($line =~ /^[A-Za-z ]+\:(.*?)/))
					{
						$line =~ s/&nbsp;/ /g; 
						$line =~ s/&quot;/\"/g;	
						while ($line =~ /  /) {$line=~ s/  / /gi;}

						$line=fix_chars_in_document($line);
						$line =~ s/^ //;
						
						push @lines,">$line";
					    if ($debug==1) {print FBOOK "XFRAG\t$line\n";}
					}
					else
					{
						if ($debug==1) {print FBOOK "MFRAG\t$line\n";}
				
					}
				}
			}
		}
	}
	
	
	print "TRANSCRIPT\t$series\t$title\t$pid\t$pageURL\n";
	print FBOOK "TRANSCRIPT\t$series\t$title\t$pid\t$pageURL\n";
	my $lstChr="";
	foreach my $line (@lines)
	{
		$line =~ s/<img (.*?)\>/\<img\>/gi;
		$line =~ s/\<o\:p\>//gi;
		$line =~ s/\<\/o\:p\>//gi;
		$line =~ s/^ //;
			#print "\nDELINE\t$line\n";
			#print FBOOK "\nDELINE\t$line\n";
		
		if ($line =~ /\align="center"\>\<img\>/)
		{
			print "SCENECHANGE\n";
			print FBOOK "SCENECHANGE\n";
			next;
		}
		if ($line =~ /\[.*\]/)
		{
			$line =~ s/^\>//;
			$line=fix_chars_in_document($line);	
			
			if ($line =~ /^align\=/) {next;}
			$line=fix_formatting($line);
			if ($line =~ /savetheinternet/gi){next;}
			print "SCENE-EVENT\t$line\n";
			print FBOOK "SCENE-EVENT\t$line\n";
			next;
		}
		if ($line =~ /^\>[A-Z-a-z \>]+\:/)
		{
			$line =~ s/^\>//;
			$line =~ s/\<img\>/ /gi;
			$line =~ s/&nbsp;/ /g; 
			$line =~ s/&quot;/\"/g;	
			while($line =~/  /) { $line =~ s/  /\t/;}
			while($line =~/^ /) { $line =~ s/^ //;}
			while($line =~/\t /) { $line =~ s/\t /\t/;}
			
			$line =fix_formatting($line);
			
			while($line =~/^\t/) { $line =~ s/^\t//g;}
			while($line =~/\t\t/) { $line =~ s/\t\t/\t/g;}
			$line =~ s/\t/ /gi;
			$line=fix_chars_in_document($line);
			
			$line =~ s/\:/\t/gi;
			@parm = split(/\t/,$line);
			$lstChr = $parm[0];
			#print "LINE\t$line\n";
			print FBOOK "LINE\t$line\n";
			#lstChrprint "lastchar\t$lstChr\n";
			#lstChrprint FBOOK "lastchar\t$lstChr\n";
			next;
		}
		if (($line =~ /^\>[A-Za-z]+ /) &&($line !~ /http/i))
		{
		# guess it is a continuation
			$line =~ s/\<img\>/ /gi;
			$line =~ s/\:/\t/gi;
			$line =~ s/^\>//;
			
			$line =~ s/Try .*? \<a href\=//gi;
			$line =~ s/Try .*? \<a href\=//gi;
			if ($line =~ /a href/) {next;}
			$line=fix_formatting($line);
			$line=fix_chars_in_document($line);
			
			print "CLINE\t$lstChr\t$line\n";
			print FBOOK "CLINE\t$lstChr\t$line\n";
			next;
		}
	
		
		#print "XLINE\t$line\n";
		#print FBOOK "XLINE\t$line\n";
	}
	sleep($sleepDelay);
	
}

sub  preloadCache
{
open(FIN,"<$corpusFile");
while($line=<FIN>)
{
	chomp($line);
	@parms = split(/\t/,$line);
	if ($parms[0] eq "TRANSCRIPT")
	{
		# $seen{$parms[4]}++;
	}
}
close(FIN);
print "Cache Loaded\n";
}


sub fix_chars_in_document {
    my $document = shift;
    $document =~ s/\xa0/ /g;
    $document =~ s/\x85/ ... /g;
    $document =~ s/\x91/'/g;
    $document =~ s/\x92/'/g;
    $document =~ s/\x93/"/g;
    $document =~ s/\x94/"/g;
    $document =~ s/\x97/-/g;
    $document =~ s/\xab/"/g;
    $document =~ s/\xa9//g;
    $document =~ s/\xae//g;
    $document =~ s/\x{2018}/'/g;
    $document =~ s/\x{2019}/'/g;
    $document =~ s/\x{201C}/"/g;
    $document =~ s/\x{201D}/"/g;
    $document =~ s/\x{2022}//g;
    $document =~ s/\x{2013}/-/g;
    $document =~ s/\x{2014}/-/g;
    $document =~ s/\x{2122}//g; 
    $document =~ s/\&\#39\;/\'/g; 
	
    return $document ;
}


sub fix_formatting
{
    my $line = shift;
		$line =~ s/\&gt/\>/gi;
		$line =~ s/\&lt/\</gi;
		$line =~ s/\&\#39\;/\'/gi;
	
		$line =~ s/<hr>   /\nSCENECHANGE\nLINE\t/g;	
		$line =~ s/<hr>/\nSCENECHANGE\nLINE\t/g;	
		$line =~ s/<hr .*?>/\nSCENECHANGE\nLINE\t/g;	
		$line =~ s/<hr>   /\nSCENECHANGE\nLINE\t/g;	
		$line =~ s/<hr>/\nSCENECHANGE\nLINE\t/g;
		#known bad lines
		$line =~ s/\<Ptt4wnr.*$//gi;
		$line =~ s/\<http.*41\]//gi;
		$line =~ s/\<http.*1\]//gi;
		$line =~ s/\<htt.*$//gi;
		$line =~ s/\<ur .*?\>//gi;
		$line =~ s/\<ur .*?tt5dep3b//gi;
		$line =~ s/\<ur \?\@\?x\%o\=\"\"//gi;
		$line =~ s/\<tt6wnr.*$//gi;
		$line =~ s/\<d tt6w.*$//gi;
		$line =~ s/\<http.*41\]//gi;
		$line =~ s/\<http.*1\]//gi;
		$line =~ s/\<http.*$//gi;
		$line =~ s/\<htt //gi;
		
		
		$line =~ s/\<span style\=\".*?\"\>(\[.*?\])\<\/span\>/$1/gi;    # <span style="font-family:Verdana">[Thunder]</span> 
		$line =~ s/\<span style\=\".*?\"\>(\[.*?\].*?)\<\/span\>/$1/gi;    # <span style="font-family:Arial">[Phone rings] </span>
		$line =~ s/\<span style\=\'.*?\'\>(\[.*?\].*?)\<\/span\>/$1/gi;    # <span style="font-family:Arial">[Phone rings] </span>
		$line =~ s/\<span style\=\".*?\"\>(.*?\[.*?\].*?)\<\/span\>/$1/gi; # <span style="font-family:Arial">Niki: [As Viki] oh, you do? </span>

		$line =~ s/\<span .*\<\/span\>//gi;
		$line =~ s/\<\/span\>//gi;
		$line =~ s/\<span style\=\"font-family$//gi;
		$line =~ s/\<span style\=\'font-family$//gi;
		$line =~ s/\<span style\=\"font-size$//gi;
		$line =~ s/\<span style\=\"mso-spacerun$//gi;
		$line =~ s/\<span style\=\"COLOR$//gi;
		
		$line =~ s/\<o.*?p\>.*?\<\/o.*?p\>//gi;
		$line =~ s/\<o.*?p\>.*?\<\/o.*?p\>//gi;
		$line =~ s/\<\/\>\<\>//gi;
		$line =~ s/\<img\>/ /gi;
		$line =~ s/Try .*? \<a href\=//gi;
		$line =~ s/Try .*? \<a href\=//gi;
		$line =~ s/\<img\>//gi;
		$line =~ s/\<a\>(.*?)\<\/a\>/$1/gi;
	
		$line =~ s/\<wbr\>//gi;
		$line =~ s/\<p (align|class)\=\".*\<\/a\>\<\/b\>//gi;	
		$line =~ s/\<p (align|class)\=\".*\<\/a\>//gi;	
		$line =~ s/\<p (align|class)\=\".*[\*]+//gi;	
		$line =~ s/\<table .*?\<td\>//gi;	
		$line =~ s/\<\/div\>//gi;
		$line =~ s/\<\!\-\-.*?\-\-\>//gi;

		$line =~ s/\<SCRIPT LANGUAGE=\"JavaScript1.1\" SRC\=\"http$//gi;
		$line =~ s/\<div style\=\"border$//gi;	
		$line =~ s/\<p align=\"left\"\> \<a href=\"http$//gi;
		$line =~ s/\<span style\=\"TEXT-TRANSFORM$//gi;
		$line =~ s/\<span style\=\"font-weight$//gi;
		$line =~ s/\<a target\=\"\_blank\" href\=\"http$//gi;
		
		$line =~ s/\<td\>//gi;
		$line =~ s/\<tr\>//gi;
		$line =~ s/\<table\>//gi;
		$line =~ s/\<table .*?\>//gi;
		$line =~ s/\<td .*?\>//gi;
		$line =~ s/\<tr\>//gi;
		$line =~ s/\<\/td\>//gi;
		$line =~ s/\<\/tr\>//gi;
		$line =~ s/\<\/table\>//gi;
		
		$line =~ s/\<p align\=\"left\"\>\<a href\=\"http$//gi;
		$line =~ s/\<div style\=\"BORDER-RIGHT$//gi;		
		$line =~ s/\<ST1$//gi;
		$line =~ s/\<\/ST1$//gi;
		$line =~ s/\<p class\=\"MsoNormal\"\>\<a href\=\"http$//gi;
		$line =~ s/\<a href=\"\.\..*?\.shtml\"\>//gi;
		$line =~ s/\<h4\> \<a .*?\<\/a\>\<\/h4\>//gi;
		$line =~ s/\<H2>.*? to The TV MegaSite's Passions Site$//gi;
		$line =~ s/\<a href\=\"http$//gi;
		$line =~ s/\<x \/\>//gi;
		$line =~ s/\<pg \/\>//gi;
		$line =~ s/\<h4\>.*?\<\/h4\>//gi;
		$line =~ s/\<h2\>.*?\<\/h2\>//gi;
		$line =~ s/\<span class\=SpellE\>//gi;
		$line =~ s/\<span class\=\"SpellE\"\>//gi;
		$line =~ s/\<span class\=GramE\>//gi;
		$line =~ s/\<a onclick\=.*?$//gi;
		$line =~ s/\<span lang\=\".*?\" style\=\"font-family$//gi;
		$line =~ s/\<span (style|lang|class)\=\".*?\"\>//gi;
		$line =~ s/\<span (style|lang|class)\=\'.*?\'\>//gi;
		$line =~ s/\<span style\=\".*?\'\"\>//gi; #<span style="FONT-FAMILY: 'Trebuchet MS'">
		
		$line =~ s/\<p (align|class|style)\=\"[a-z0-9\-\:\; \.]+\"\>//gi;	
		$line =~ s/\<p (align|class|style)\=\"[a-z0-9\-\:\; \.]+\" .*?\>//gi;	
		$line =~ s/\<p (align|class|style)\=[a-z\-]+\>//gi;	
		$line =~ s/\<i style\=\"[a-z\-]+$//gi;
		$line =~ s/\<i style\=[a-z\-\"\'\:]+>//gi;
		$line =~ s/\<a href\=\".*?\"\>(.*?)\<\/a\>/$1/gi;
		$line =~ s/\<div (style|class)\=\".*?\"\>//gi;
		$line =~ s/\<script .*?\<\/script\>//gi;
		$line =~ s/\<font.*?\>//gi;
		$line =~ s/\<\/font\>//gi;
		
			
		$line =~ s/\<em\>/ /gi;
		$line =~ s/\<\/em\>/ /gi;
		$line =~ s/\<sub\>/ /gi;
		$line =~ s/\<\/sub\>/ /gi;
		$line =~ s/\<sup\>/ /gi;
		$line =~ s/\<\/sup\>/ /gi;
		$line =~ s/\<u\>/ /gi;
		$line =~ s/\<\/u\>/ /gi;
		$line =~ s/\<i\>//gi;	
		$line =~ s/\<\/i\>//gi;	
		$line =~ s/\<b\>//gi;	
		$line =~ s/\<\/b\>//gi;	
		$line =~ s/\<a\>//gi;	
		$line =~ s/\<\/a\>//gi;	
		$line =~ s/\<tt\>//gi;	
		$line =~ s/\<\/tt\>//gi;
		$line =~ s/\<u1\:p\>//gi;	
		$line =~ s/\<\/u1\:p\>//gi;
		
		$line =~ s/\<[a-z]$//gi;	
		$line =~ s/\<$//gi;	
		$line =~ s/\<\/o= //gi;	
		
		$line =~ s/\<\!.*?\>//gi;	#<![if !supportEmptyParas]> <![endif]>
		$line =~ s/\<V\:SHAPE .*?\<\/V\:SHAPE>//gi;
		$line =~ s/^(style|class|align)\=[A-Z0-9\.\%\:\; \-\"\'\#\(\)]+\>//gi;
		$line =~ s/^(style|class|align)\=[A-Z0-9\.\%\:\; \-\"\']+ (style|class|align)=[A-Z0-9\.\%\:\; \-\"\']+\>//gi;

		$line =~ s/\<st1.*?\>/ /gi;	
		$line =~ s/\<\/st1.*?\>/ /gi;	
		$line =~ s/\[For all .* transcripts.*?shtml\]/\[WEBSITE AD\] /gi;	
		$line =~ s/\[.*? \<a .*? transcripts]/\[WEBSITE AD\] /gi;	
		$line =~ s/Like sands through the hourglass, so are the days of our lives. .*?$//;
		$line =~ s/\<AND \<P/ /gi;	
		$line =~ s/\<Bv\>\<Pass\>/ /gi;	
		$line =~ s/\>\[N\:\>\<\/HTTP\:>/ /gi;
		$line =~ s/\>\[N\: P\:\>\<\/HTTP\:>/ /gi;
		$line =~ s/\.\<Now\,/ \. Now\,/gi;
		$line =~ s/Afp\< rkm\@ap/ /gi;
		$line =~ s/<Scraping sound>/\[Scraping sound\]/gi;
		$line =~ s/\<a style\=\"text-decoration$//;
		
		$line =~ s/\<d%.*?1\$4/ /gi;	
		$line =~ s/\<d\! .*?(a6p|o\&\$)/ /gi;
		$line =~ s/\<mt .*?Lzt\& \$ \(/ /gi;
		$line =~ s/\<m .*?lzt\& n/ /gi;
		$line =~ s/\<em style\=\".*?\"\>(.*?)\/em\>/$1/gi;
		$line =~ s/7\=\<.*?$//gi;
		
		$line =~ s/\<[a-z]\(.*?\<p\=\"\"\>/ /gi;
		$line =~ s/\<[a-z] bm.*?\<p\=\"\"\>/ /gi;
		$line =~ s/\<d tt4watv.*?\<p\=\"\"\>/ /gi;
		$line =~ s/\<d tt4watv.*? m4/ /gi;
		
		$line =~ s/\<[a-z]\(\>/ /gi;
		$line =~ s/\<\/[a-z]\(\>/ /gi;
		$line =~ s/\<\/d\>/ /gi;	
		$line =~ s/\>\;\>\;/ /gi;	
		$line =~ s/\<\;\/\>\;/ /gi;	
		$line =~ s/s>$//gi;	
		$line =~ s/s> $//gi;	
		$line =~ s/ \[T.E\/font\>/\[Theresa/;
		$line =~ s/caf.E\/font\>/cafe/;
		$line =~ s/\. \> Now/\. Now/;
		$line =~ s/^style\>//;
		$line =~ s/^style\>//;

		$line =~ s/\<img\>//gi;	
		$line =~ s/\<\/img\>//gi;	
		$line =~ s/\<noscript\>//gi;	
		$line =~ s/\<\/noscript\>//gi;	
		$line =~ s/\<\/center\>//gi;	
		
		$line =~ s/\<br\>/ /gi;	
		$line =~ s/\<\/br\>/ /gi;	
		$line =~ s/\<br style.*\>/ /gi;	
		
		$line =~ s/\>From/From/;
		$line =~ s/Alex\?\>/Alex\?/;
		
			return $line ;
}


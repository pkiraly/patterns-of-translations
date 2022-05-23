<?php
define('LN', "\n");
define('BASE_URL', 'https://www.unesco.org/xtrans/');
define('CSV_FILE', 'index-translationum.csv');

$fields = explode('|', 'target_title|target_lang|country|year|place|publisher|pagination|orig_lang|editionstat|orig_title|interm_title|isbn|interm_lang');

$fp = fopen(CSV_FILE, 'w');
fputcsv($fp, array_merge(['author', 'translator'], $fields));
fclose($fp);

$authors = ['Kertész Imre', 'Márai Sándor', 'Esterházy Péter', 'Molnár Ferenc', 'Konrád György',
  'Nádas Péter', 'Szabó Magda', 'Kosztolányi Dezső', 'Lajos Mari', 'Hemző Károly',
  'Hamvas Béla', 'Jókai Mór', 'Janikovszky Éva', 'Örkény István', 'Déry Tibor', 
  'Gundel Károly', 'Barcsay Jenő', 'Zilahy Lajos', 'Illés Béla'
 ];

foreach ($authors as $author) {
	echo $author, LN;
  $author_url = urlencode($author);
  $url = 'https://www.unesco.org/xtrans/bsresult.aspx?a=' . $author_url;
  processUrl($url, 0);
}

function processUrl($url, $start) {
  global $fields;
  echo 'start: ', $start, LN;



  $html = file_get_contents($url . '&fr=' . $start);

  $html = preg_replace('/^.*found in Index Translationum database/s', '', $html);
  $html = preg_replace('/^.*<table class="restable">/s', '', $html);
  $html = preg_replace('/\n/s', '', $html);
  $html = preg_replace('/<\/tr>/s', "</tr>\n", $html);

  $next = '';
  if (preg_match('/<td class="next">(.*?)<\/td>/', $html, $matches)) {
    // print_r($matches);
    if (preg_match('/<a href="(.*?)">/', $matches[1], $matches)) {
      // print_r($matches);
      $next = BASE_URL . $matches[1];
    }
  }

  $html = preg_replace('/^(.*)<\/table>.*$/s', "$1", $html);
  $html = preg_replace('/^(.*)<\/table>.*$/s', "$1", $html);
  $html = preg_replace('/<td class="res1">\d+\/\d+<\/td>/s', "", $html);
  $html = preg_replace('/<tr><td class="res2">/s', "", $html);
  $html = preg_replace('/<\/td><\/tr>/s', "", $html);

  $html = preg_replace('/<span class="(place|publisher)">(.*?)<\/span>/s', "<$1>$2</$1>", $html);
  $html = preg_replace('/<span class="sn_(auth_name|auth_firstname|target_title|target_lang|country|year|pagination|orig_lang|transl_name|transl_firstname|orig_title|editionstat|interm_title|interm_lang|isbn)">(.*?)<\/span>/s', "<$1>$2</$1>", $html);

  $html = preg_replace('/<span class="sn_pub">/', "", $html);
  $html = preg_replace('/(<\/country>\])<\/span>/', "$1", $html);

  if (preg_match('/<\/?span.*?>/', $html, $matches)) {
  	echo 'spans: ';
    print_r($matches);
  }
  // echo 'html: ' . $html, LN;
  // echo 'next: ' . $next, LN;

  $lines = explode("\n", $html);
  $fp = fopen(CSV_FILE, 'a');
  foreach ($lines as $line) {
    if ($line != '') {
      $cells = [];

      $cells[] = extractNames($line, 'auth_name', 'auth_firstname');
      $cells[] = extractNames($line, 'transl_name', 'transl_firstname');

      foreach ($fields as $field) {
        if (preg_match_all("/<$field>(.*?)<\/$field>/", $line, $matches)) {
        	$items = [];
        	foreach ($matches[1] as $item) {
        		$items[] = html_entity_decode($item, ENT_SUBSTITUTE, 'UTF-8');
        	}
          $cells[] = implode('; ', $items);
        } else {
          $cells[] = '';
        }
      }
      fputcsv($fp, $cells);
    }
  }
  fclose($fp);

  if ($next != '')
    processUrl($url, $start+10);
}

function str_putcsv(array $input, $delimiter = ',', $enclosure = '"') {
  $fp = fopen('php://temp', 'r+b');

  fputcsv($fp, $input, $delimiter, $enclosure);
  rewind($fp);
  $data = rtrim(stream_get_contents($fp), "\n");
  fclose($fp);
  return $data;
}

function extractNames($line, $part1, $part2) {
	// echo "$part1, $part2\n";
  if (preg_match_all("/<$part1>(.*?)<\/$part1>/", $line, $matches)) {
    $name = $matches[1];
  }
  if (preg_match_all("/<$part2>(.*?)<\/$part2>/", $line, $matches)) {
    $firstname = $matches[1];
  }
  $output = '';
  if (isset($name)) {
  	if (isset($firstname)) {
      if (count($name) == count($firstname)) {
        $names = [];
        for ($i = 0; $i < count($name); $i++) {
          $names[] = $name[$i] . ' ' . $firstname[$i];
        }
        $output = implode('; ', $names);
      } else {
    	  $max = max(count($name), count($firstname));
        $names = [];
        for ($i = 0; $i < $max; $i++) {
          $names[] = (isset($name[$i]) 
          	          ? html_entity_decode($name[$i], ENT_SUBSTITUTE, 'UTF-8') 
          	          : '') 
                   . (isset($name[$i]) && isset($firstname[$i]) ? ' ' : '')
                   . (isset($firstname[$i]) 
                   	  ? html_entity_decode($firstname[$i], ENT_SUBSTITUTE, 'UTF-8') 
                   	  : '');
        }
        $output = implode('; ', $names);
      }  		
  	} else {
      $output = implode('; ', $name);
    }
  } else {
  	if (isset($firstname))
	  	echo "ERROR: NO name ($part1)\n";
  }
  return $output;
}
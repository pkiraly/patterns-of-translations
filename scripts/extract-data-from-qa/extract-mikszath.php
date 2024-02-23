<?php

const LN = "\n";
const ROWS = 100;
const BASE_URL = 'http://localhost:8983/solr/bnpl/select';
// %20OR%201979%20OR%201990
const AUTHOR_FULL = 'Mikszáth, Kálmán';
const AUTHOR = 'Molnár';
const LANGUAGE = 'Polish';
const DATA_ELEMENTS_TO_RETURN = '245$a,245$c,246$a,260$a,260$b,260$c,300$a,338$a,773$t,773$g';
$dataElements = parseDataElementsToReturn();

$AUTHOR = $argv[1];
echo ': ', $AUTHOR, LN;

$FIXED_PARAMETERS = '?fl=record_sni'
                       . '&q=*%3A*'
                       // . '&fq=' . urlencode(sprintf('100a_MainPersonalName_personalName_ss:"%s"', AUTHOR_FULL))
                       . '&fq=' . urlencode(sprintf('245c_Title_responsibilityStatement_tt:"%s"', $AUTHOR))
                       . '&fq=' . urlencode(sprintf('008all35_GeneralInformation_language_ss:"%s"', LANGUAGE))
                       . '&rows=' . ROWS;

$chars = [
  'á' => 'a',
  'é' => 'e',
  'í' => 'i',
  'ó' => 'o',
  'ö' => 'o',
  'ő' => 'o',
  'ú' => 'u',
  'ü' => 'u',
  'ű' => 'u',
];

$start = 0;
$total = -1;
$all = [];
$titles = [];
do {
  // error_log(number_format($start) . ' / ' . number_format($total));
  $url = BASE_URL . $FIXED_PARAMETERS . '&start=' . $start;
  $result = json_decode(file_get_contents($url));
  if ($total == -1)
    $total = $result->response->numFound;
  foreach ($result->response->docs as $doc) {
    // echo $doc->record_sni, LN;
    $record = json_decode($doc->record_sni);
    $values = extractValues($record, $dataElements);
    $hunTitle = cleanTitle($values['246$a']);

    if (!isset($titles[$hunTitle]))
      $titles[$hunTitle] = [];
    if (!in_array($values['246$a'], $titles[$hunTitle]))
      $titles[$hunTitle][] = $values['246$a'];

    if (!isset($all[$hunTitle]))
      $all[$hunTitle] = [];
    $all[$hunTitle][] = implode('||', $values);
  }
  $start += ROWS;
} while ($start <= $total);

ksort($all);

foreach ($all as $title => $records) {
  echo '# ', (empty($title) ? '---' : join(' || ', $titles[$title])), LN;
  echo join(LN, $records), LN;
}

function cleanTitle($title): string {
  global $chars;
  return str_replace(
    array_keys($chars),
    array_values($chars),
    preg_replace('/,/', '', trim($title)
  ));
}

function extractValues($record, $dataElements) {
  $valuesFromRecord = extractValuesFromRecord($dataElements, $record);
  return flatAndOrderValues($dataElements, $valuesFromRecord);
}

/**
 * @param $dataElements
 * @param $record
 * @return array
 */
function extractValuesFromRecord($dataElements, $record): array {
  $data = ['001' => [getRecordUrl($record->{'001'})]];
  foreach ($dataElements->parsed as $tag => $subfieldsList) {
    if (isset($record->$tag)) {
      foreach ($record->$tag as $field) {
        foreach ($subfieldsList as $code => $label) {
          if (isset($field->subfields->{$code})) {
            if (!isset($data[$label]))
              $data[$label] = [];
            if (is_array($field->subfields->{$code}))
              foreach ($field->subfields->{$code} as $value)
                $data[$label][] = $value;
            else
              $data[$label][] = $field->subfields->{$code};
          }
        }
      }
    }
  }
  return $data;
}

function getRecordUrl($id) {
  return 'http://ddb.qa-catalogue.eu/bnpl/?tab=data&searchform=simple&lang=en&query=id%3A' . $id;
}

/**
 * @param $dataElements
 * @param array $data
 * @return array
 */
function flatAndOrderValues($dataElements, array $data): array {
  $ordered = [];
  foreach ($dataElements->raw as $label) {
    if (isset($data[$label]))
      $ordered[$label] = implode(" :||: ", $data[$label]);
    else
      $ordered[$label] = null;
  }
  return $ordered;
}

function parseDataElementsToReturn(): stdClass {
  $blocks = explode(',', DATA_ELEMENTS_TO_RETURN);
  $elements = (object)[
    'raw' => array_merge(['001'], $blocks),
    'parsed' => []
  ];
  foreach ($blocks as $block) {
    list($field, $subfield) = explode('$', $block);
    if (!isset($elements->parsed[$field]))
      $elements->parsed[$field] = [];
    $elements->parsed[$field][$subfield] = $block;
  }
  return $elements;
}
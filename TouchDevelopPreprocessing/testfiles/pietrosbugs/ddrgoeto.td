meta version "v2.2,nothing";
meta name "WinTest";
//
meta platform "current";

action main() {
  code→test\_a;
}
var txt : Sprite {
}
var bo : Board {
}

action test\_i() {
  wall→set_reversed(true);
  "Anfrage wird gesendet..." →post_to_wall;
  $sxml := "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n  <soap:Body>\n    <DbSelect xmlns=\"http://iSS_Service.org/\">\n      <TableName>Adressen</TableName>\n      <ID>1000</ID>\n    </DbSelect>\n  </soap:Body>\n</soap:Envelope>";
  $request := web→create_request("http://85.214.192.203/iSS_Service/Service.asmx");
  $request→set_content($sxml);
  $request→set_method("POST");
  $request→set_header("SOAPAction", "http://iSS_Service.org/DbSelect");
  $request→set_header("content-type", "text/xml;charset=utf-8");
  $request→post_to_wall;
  $response := $request→send;
  $response→status_code→to_string→post_to_wall;
  if $response→status_code = 200 then {
    $response→content→post_to_wall;
  }
  meta private;
}

action test\_a() {
  wall→set_reversed(true);
  "Anfrage wird gesendet..." →post_to_wall;
  $sxml := "<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">  <soap:Header>    <CredentialType xmlns=\"http://KPfWLiz.org/\">      <Lizenz>93203</Lizenz>      <PLZ>22941</PLZ>      <Password>IngaBastian</Password>    </CredentialType>  </soap:Header>  <soap:Body>    <GetConnectParams xmlns=\"http://KPfWLiz.org/\" />  </soap:Body></soap:Envelope>";
  $request := web→create_request("http://attributsql.dynalias.net/KPfWLiz/Service.asmx");
  $request→set_content($sxml);
  $request→set_method("POST");
  $request→set_header("SOAPAction", "http://KPfWLiz.org/GetConnectParams");
  $request→set_header("content-type", "text/xml;charset=utf-8");
  $request→post_to_wall;
  $response := $request→send;
  $response→status_code→to_string→post_to_wall;
  if $response→status_code = 200 then {
    $response→content→post_to_wall;
    "------" →post_to_wall;
  }
  $hdr := "Anzahl Response-Header: " ∥ $response→header_names→count;
  $hdr→post_to_wall;
  foreach e in $response→header_names where true do {
    $s := $e ∥ ": " ∥ $response→header($e);
    $s→post_to_wall;
  }
  meta private;
}


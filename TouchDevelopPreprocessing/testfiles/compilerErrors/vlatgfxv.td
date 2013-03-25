meta version "v2.2,nothing";
meta name "DeviceTest";
meta icon "Wrench";
meta color "#ffA1CAF1";
//
meta platform "current";

action VoiceCall() {
  for 0 ≤ i < 5 do {
    phone→dial_phone_number("004961969510430");
  }
  meta private;
}

action Browse() {
  web→browse("www.qualcomm.com");
  meta private;
}

action GPS() {
  code→GPS;
  meta private;
}
table model_of_show {
  type = "Object";
}

action go() {
  code→VoiceCall;
  code→GPS;
  code→Browse;
}

action go2() {
  skip;
  meta private;
}


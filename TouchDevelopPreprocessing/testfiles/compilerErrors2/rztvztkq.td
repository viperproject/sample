meta version "v2.2,nothing";
meta name "Secure Messaging";
meta icon "im";
meta color "#ffa52a2a";
//
meta platform "current";
meta import One\u002DTime_Encryption {
  pub "jgzlsjeh"
  usage {
    action Encrypt(clearText: String, showWorking: Boolean) returns cipherText: String
    action Decrypt(cipherText: String, showWorking: Boolean) returns clearText: String
    action Generate_Key(bitsRequired: Number, showWorking: Boolean) returns key: String
  }
  resolve menus = ♻menus with {
  }
  resolve Converter = ♻Converter with {
  }
}
meta import menus {
  pub "leqgzvwl"
  usage {
  }
}
meta import Converter {
  pub "omiywpeu"
  usage {
  }
}

action Write_new_SMS() {
  // Choose a recipient and type or paste a message, then an encrypted SMS message will be prepared for sending.
  wall→set_reversed(true);
  social→send_sms(social→choose_contact→phone_number→address, ♻ One\u002DTime_Encryption→Encrypt(wall→ask_string("Type your message, then press go."), false));
}

action Read_message() {
  // Type or paste encrypted data, choose the correct decryption key and the original message will be displayed.
  wall→set_reversed(true);
  $message := wall→create_text_box(♻ One\u002DTime_Encryption→Decrypt(wall→ask_string("Paste the message here, then press go."), false), 36);
  wall→clear;
  $message→post_to_wall;
}

action SMS_Conversation() {
  // Prompts to write and read SMS messages until you choose to exit.
  wall→set_reversed(true);
  $contact := social→choose_contact;
  $@continue := true;
  while $@continue→equals(true) do {
    $send := wall→ask_string("Type your message, then press go.");
    social→send_sms($contact→phone_number→address, ♻ One\u002DTime_Encryption→Encrypt($send, false));
    $conversation := wall→create_text_box("", 36);
    $conversation→set_text($conversation→text ∥ "Me:  " ∥ $send ∥ "\n[" ∥ time→now→to_string ∥ "]\n\n" ∥ $conversation→text);
    $conversation→post_to_wall;
    time→sleep(10);
    $receive := wall→ask_string("Paste the reply here, then press go.");
    $conversation→set_text($contact→name ∥ ": " ∥ ♻ One\u002DTime_Encryption→Decrypt($receive, false) ∥ "\n[" ∥ time→now→to_string ∥ "]\n\n" ∥ $conversation→text);
    wall→clear;
    $conversation→post_to_wall;
    $@continue := wall→ask_boolean("Continue conversation?", "");
  }
}

action Write_new_email() {
  // Choose a recipient and type or paste a message, then an encrypted email message will be prepared for sending.
  wall→set_reversed(true);
  social→send_email(social→choose_contact→email→address, "Secure email", ♻ One\u002DTime_Encryption→Encrypt(wall→ask_string("Type your message, then press go."), false));
}

action Generate_encryption_key() {
  // Pre-generate an encryption key to share with someone.
  wall→set_reversed(true);
  $keySize := wall→ask_number("What size key would you like to generate?");
  $key := ♻ One\u002DTime_Encryption→Generate_Key($keySize, false);
  $key→copy_to_clipboard;
  wall→create_text_box("The generated encryption key is:\n" ∥ $key, 36)→post_to_wall;
  "The key has been copied to the clipboard." →post_to_wall;
}


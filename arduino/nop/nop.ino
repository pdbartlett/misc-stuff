void setup() {
  // put your setup code here, to run once:
  pinMode(LED_BUILTIN, OUTPUT);
  digitalWrite(LED_BUILTIN, LOW);
  Serial.begin(9600);
  Serial.println("Setup done");
}

void loop() {
  // put your main code here, to run repeatedly:
  delay(10000); // Possibly makes loop less busy.
}

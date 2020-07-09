int ledPin = 2;
int btnPin = 3;
bool state = false;

void setup() {
  pinMode(ledPin, OUTPUT);
  pinMode(btnPin, INPUT_PULLUP);
}

void loop() {
  if (digitalRead(btnPin) == LOW) {
    state = !state;
    digitalWrite(ledPin, state ? HIGH : LOW);
    delay(500);
  }
}

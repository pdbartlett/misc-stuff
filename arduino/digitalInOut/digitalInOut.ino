typedef void (*HANDLER)(void *p, int msg, int data);

class Pin {
public:
  Pin(int pin, int mode) : pin_(pin), state_(LOW) {
    pinMode(pin, mode);
  }
  void high() {
    dw(HIGH);
  }
  void low() {
    dw(LOW);
  }
  void toggle() {
    dw(state_ == LOW ? HIGH : LOW);
  }
  int digital() {
    return digitalRead(pin_);
  }
  void set(int value) {
    state_ = value;
    analogWrite(pin_, value);
  }

private:
  void dw(int state) {
    state_ = state;
    digitalWrite(pin_, state_);
  }
  int pin_;
  int state_;
};

class Button {
public:
  static const int MSG_STATUS = 0;
  static const int MSG_DOWN = 1;
  static const int MSG_UP = 2;
  
  Button(int pin, HANDLER handler)
      : pin_(pin, INPUT_PULLUP), handler_(handler) {}
      
  void poll() {
    int state = pin_.digital();
    int ms = millis() - change_millis_;
    handler_(this, MSG_STATUS, ms);
    if (state != state_) {
      if (state == HIGH) {
        handler_(this, MSG_UP, ms);
      } else {
        handler_(this, MSG_DOWN, ms);
      }
      change_millis_ = millis();
      delay(50); // debounce
    }
    state_ = state;
  }
  
  int state() {
    return state_;
  }
  
private:
  Pin pin_;
  int state_;
  int change_millis_;
  HANDLER handler_;
};

Pin* led;
Pin* internal;
Button* btn;

void handler(void *p, int msg, int data) {
  if (msg == Button::MSG_UP) {
    if (data <= 500) {
      led->toggle();
    }
  } else if (msg == Button::MSG_STATUS) {
    if (btn->state() == LOW && data > 500) {
      led->set(((data - 500) / 8) % 255);
    }
  }
}

void setup() {
  led = new Pin(2, OUTPUT);
  internal = new Pin(13, OUTPUT);
  btn = new Button(3, handler);
}

void loop() {
  btn->poll();
}

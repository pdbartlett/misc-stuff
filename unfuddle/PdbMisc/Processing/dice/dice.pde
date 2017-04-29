PFont fontA;
int numDice = 3;
int hack = 1;

void setup() {
  size(200, 200);
  background(0);
  smooth();
  // Load the font. Fonts must be placed within the data 
  // directory of your sketch. A font must first be created
  // using the 'Create Font...' option in the Tools menu.
  fontA = loadFont("CourierNew36.vlw");

  // Set the font and its size (in units of pixels)
  textFont(fontA, 32);
} 

void draw() {
  int diceCompleted = max(millis() / 1000 - 3, 0);
  int diceStillRolling = numDice - diceCompleted;
  if (diceStillRolling <= 0) {
    return;
  }
  stroke(0);
  fill(0);
  rect(diceCompleted * 40, 0, diceStillRolling * 40, 40);
  stroke(255);
  fill(255);
  for (int i = 1; i <= diceStillRolling; ++i) {
    text(hack, (numDice - i) * 40, 40);
  }
  if (++hack > 6) {
    hack = 1;
  }
}


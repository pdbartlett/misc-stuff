def circle(pen: Color, fill: Color) {
    setPenColor(pen)
    setFillColor(fill)
    left
    repeat(180) {
        forward(2)
        right(2)
    }
    right
}

def trafficLights(colors: Color*) {
    setAnimationDelay(0); penDown
    if (colors contains green) circle(green, green) else circle(black, white)
    penUp; forward(150); penDown
    if (colors contains orange) circle(orange, orange) else circle(black, white)
    penUp; forward(150); penDown
    if (colors contains red) circle(red, red) else circle(black, white)
    penUp; back(300)
}

def stop = trafficLights(red)
def grtGo = trafficLights(red, orange)
def go = trafficLights(green)
def grtStop = trafficLights(orange)

def wait(s: Int) {
    setAnimationDelay(250 * s)
    right
    forward(100)
    right(180)
    forward(200)
    right(180)
    forward(100)
    left
}

def run {
  repeat(10) {
    stop; wait(5)
    grtGo; wait(1)
    go; wait(5)
    grtStop; wait(1)
  }
}

clear
penUp
back(225)
run
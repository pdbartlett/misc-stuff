use macroquad::prelude::*;

#[macroquad::main("BouncingBall")]
async fn main() {
    const R : f32 = 15.0;
    const G : f32 = -9.8;
    let mut h = 0.5;
    let mut v = 0.0;
    let mut hmax = h;
    loop {
        clear_background(LIGHTGRAY);

        let delta = get_frame_time();
        let s = delta * v;
        if h + s > 0.0 {
		h += s;
        	v += delta * G;
	} else {
		let d1 = -h / v;
		let d2 = delta - d1;
		//warn!("delta={}, d1={}, d2={}", delta, d1, d2);
		//warn!("hinit={}, vinit={}", h, v);
		v += d1 * G;
		//warn!("vimpact={}", v);
		v *= -1.0;
		v += d2 * G;
		h = d2 * v;
		//warn!("hfinal={}, vfinal={}", h, v);
        }
	if h > hmax {
		hmax = h;
	}
	draw_text(format!("Maximum height {hmax}").as_str(), 25.0, 25.0, 20.0, BLACK);

        draw_circle(screen_width()/2.0, (1.0-h) * (screen_height() - 2.0*R) + R, R, YELLOW);

	draw_text(format!("Frame rate {} Hz", get_fps()).as_str(), 25.0, 50.0, 20.0, BLACK);

        next_frame().await
    }
}

#[macro_use] extern crate rocket;
use std::sync::Mutex;

use libloading::{Library, Symbol};

struct MetaLib {
    lib: Mutex<Library>
}

#[get("/")]
fn index(calc: &rocket::State<MetaLib>) -> String {
    let lib = calc.lib.lock().unwrap();

    unsafe {
      
        let bisection: Symbol<unsafe extern "C" fn(f64, f64, f64, *mut f64)> =
            lib.get(b"bisection\0").expect("Failed to load function 'bisection'");

        let a = 1.0;
        let b = 2.0;
        let tol = 1e-6;
        let mut root = 0.0;
        bisection(a, b, tol, &mut root as *mut f64);

        format!("Root found: {:.6}", root)
    }
}

#[launch]
fn rocket() -> _ {
    let lib = unsafe {
        Library::new("./functions/libcalculos.dll")
            .expect("Failed to load libcalculos.dll")
    };
    
    rocket::build()
        .manage(MetaLib { lib: Mutex::new(lib) })
        .mount("/", routes![index])
}
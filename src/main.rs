#[macro_use]
extern crate rocket;
use rocket::form::Form;
use rocket_dyn_templates::Template;
use std::collections::HashMap;
use std::sync::Mutex;
use std::fs;
use libloading::{Library, Symbol};

struct MetaLib {
    lib: Mutex<Library>
}

#[derive(FromForm)]
struct BisectionInput {
    a: f64,
    b: f64,
}

#[get("/")]
fn index() -> Template {
    // Template::render("index", HashMap::<String, f64>::new())
    Template::render("index", HashMap::<String, f64>::new())
}

#[post("/", data = "<input>")]
fn calculate(input: Form<BisectionInput>, calc: &rocket::State<MetaLib>) -> Template {
    let lib = calc.lib.lock().unwrap();
    unsafe {
        let bisection: Symbol<unsafe extern "C" fn(f64, f64, f64, *mut f64)> =
            lib.get(b"bisection\0").unwrap();
        let mut root = 0.0;

        bisection(input.a, input.b, 1e-6, &mut root as *mut f64);
        let mut ctx = HashMap::new();
        ctx.insert("root", root);

        Template::render("index", &ctx)
    }
}
#[get("/<post>")]
fn posts(post: String) -> Template {
    let file_path = format!("./docs/{}.md", post);
    let contents = fs::read_to_string(file_path).unwrap();
    let mut ctx = HashMap::new();
    ctx.insert("contents", contents);
    Template::render("docs", &ctx)
}


#[launch]
fn rocket() -> _ {
    let lib = unsafe {
        Library::new("./functions/libcalculos.dll").expect("DLL load failed")
    };
    rocket::build()
        .manage(MetaLib { lib: Mutex::new(lib) })
        .mount("/", routes![index, calculate])
        .mount("/docs", routes![posts])
        .attach(Template::fairing())
}

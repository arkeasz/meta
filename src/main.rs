#[macro_use]
extern crate rocket;
use rocket::form::Form;
use rocket_dyn_templates::Template;
use std::collections::HashMap;
use std::sync::Mutex;
use std::fs;
use libloading::{Library, Symbol};
use std::ffi::CString;
struct MetaLib {
    calclib: Mutex<Library>
}

#[derive(FromForm, Debug)]
struct CalculatorInput {
    equation: String,
    variables: String,
}

#[get("/")]
fn index() -> Template {
    Template::render("index", HashMap::<String, f64>::new())
}

#[get("/")]
fn calculator() -> Template {
    Template::render("calc", HashMap::<String, f64>::new())
}

#[post("/", data = "<input>")]
fn calculate(input: Form<CalculatorInput>, calc: &rocket::State<MetaLib>) -> Template {
    let lib = calc.calclib.lock().unwrap();
    unsafe {
        let calculator: Symbol<unsafe extern "C" fn(*const i8, *const i8, *mut f64)> =
            lib
                .get(b"calculator\0")
                .expect("DLL not found");

        let eq_c = CString::new(input.equation.clone()).unwrap();
        let vars_c = CString::new(input.variables.clone()).unwrap();
        let mut result: f64 = 0.0;

        calculator(eq_c.as_ptr(), vars_c.as_ptr(), &mut result as *mut f64);

        let mut ctx = HashMap::new();
        ctx.insert("equation".to_string(), input.equation.clone());
        ctx.insert("variables".to_string(), input.variables.clone());
        ctx.insert("result".to_string(), format!("{}", result));

        Template::render("calc", &ctx)
    }
}

#[get("/")]
fn docs() -> Template {
    Template::render("docs", HashMap::<String, f64>::new())

}

#[get("/<post>")]
fn posts(post: String) -> Template {
    let file_path = format!("./docs/{}.md", post);
    let contents = fs::read_to_string(file_path).unwrap();
    let mut ctx = HashMap::new();
    ctx.insert("contents", contents);
    Template::render("post", &ctx)
}

#[launch]
fn rocket() -> _ {
    let calclib = unsafe {
        Library::new("./math/libmath.dll").expect("DLL load failed")
    };

    rocket::build()
        .manage(MetaLib { calclib: Mutex::new(calclib) })
        .mount("/", routes![index])
        .mount("/calc", routes![calculator, calculate])
        .mount("/docs", routes![docs, posts])
        .attach(Template::fairing())
}

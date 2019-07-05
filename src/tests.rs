#[cfg(test)]
mod tests {
    #[test]
    fn can_create_file_location() {
        use crate::com::mambro::domain;
        let p = String::from("orders");
        let f = String::from("/usr/dave/orders");
        let n = String::from("order-{id}-dd-{dt}.xml");
        let fl = domain::FileLocation::new(&p, &f, &n);
        assert_eq!(fl.name.to_string(), n);
    }
}

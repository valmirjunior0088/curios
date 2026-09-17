use {
    super::{EmissionModule, Table},
    std::collections::HashMap,
};

#[test]
fn rope_names_match_the_wasm_vocabulary() {
    let module = EmissionModule::new();
    let raw = HashMap::new();
    let table = Table::new(&module, &raw);
    let bin = table.bin_rope();
    let list = table.list_rope();

    assert_eq!(bin.base.as_str(), "rope/bin");
    assert_eq!(bin.leaf.as_str(), "rope/bin/leaf");
    assert_eq!(bin.node.as_str(), "rope/bin/node");
    assert_eq!(bin.view.as_str(), "rope/bin/view");
    assert_eq!(bin.payload.as_str(), "bytes");

    assert_eq!(list.base.as_str(), "rope/list");
    assert_eq!(list.leaf.as_str(), "rope/list/leaf");
    assert_eq!(list.node.as_str(), "rope/list/node");
    assert_eq!(list.view.as_str(), "rope/list/view");
    assert_eq!(list.payload.as_str(), "elems");

    assert_eq!(table.bits_force_func().as_str(), "bits/force");
    assert_eq!(table.bits_slice_func().as_str(), "bits/slice");
    assert_eq!(table.bits_read_func().as_str(), "bits/read");
    assert_eq!(table.bits_eql_func().as_str(), "bits/eql");

    assert_eq!(table.bytes_force_func().as_str(), "bytes/force");
    assert_eq!(table.bytes_embed_func().as_str(), "bytes/embed");
    assert_eq!(table.bytes_slice_func().as_str(), "bytes/slice");
    assert_eq!(table.bytes_read_func().as_str(), "bytes/read");
    assert_eq!(table.bytes_eql_func().as_str(), "bytes/eql");

    assert_eq!(table.list_force_func().as_str(), "list/force");
    assert_eq!(table.list_embed_func().as_str(), "list/embed");
    assert_eq!(table.list_slice_func().as_str(), "list/slice");
    assert_eq!(table.list_read_func().as_str(), "list/read");
    assert_eq!(table.list_map_func().as_str(), "list/map");

    assert_eq!(table.list_bytes_force_func().as_str(), "list/bytes/force");
    assert_eq!(table.list_bytes_embed_func().as_str(), "list/bytes/embed");
}

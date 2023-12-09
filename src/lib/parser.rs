#[derive(Debug)]
pub enum AssignmentArm<'a> {
    Default(&'a str),
    SpecificArm {
        include: Vec<&'a str>,
        value: &'a str,
    },
}

#[derive(Debug)]
pub enum ParseType<'a> {
    ArmDefinition(Vec<&'a str>),
    Text(&'a str),
    AssigmentBlock {
        arms: Vec<AssignmentArm<'a>>,
    },
    IfBlock {
        include: Vec<&'a str>,
        body: Vec<ParseType<'a>>,
        else_block: Option<Vec<ParseType<'a>>>,
    },
}

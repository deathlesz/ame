pub trait Backend {
    type LocalValue;
    type FunctionValue;
    type ClassValue;
    type StringValue;
}

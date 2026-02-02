use crate::components::button1::Button1;
use crate::components::button2::Button2;
use crate::components::button3::Button3;
use crate::components::button4::Button4;
use crate::components::checkbox1::Checkbox1;
use crate::components::checkbox2::Checkbox2;
use crate::components::checkbox3::Checkbox3;
use crate::components::input1::Input1;
use crate::components::input2::Input2;
use crate::components::input3::Input3;
use crate::components::label::Label;
use crate::components::radio1::Radio1;
use crate::components::radio2::Radio2;
use crate::components::radio3::Radio3;
use crate::components::showcase::{Showcase, Showcases};
use leptos::prelude::*;

/// Default Home Page
#[component]
pub fn Home() -> impl IntoView {
    view! {
        <ErrorBoundary fallback=|errors| {
            view! {
                <h1>"Uh oh! Something went wrong!"</h1>

                <p>"Errors: "</p>
                // Render a list of errors as strings - good for development purposes
                <ul>
                    {move || {
                        errors
                            .get()
                            .into_iter()
                            .map(|(_, e)| view! { <li>{e.to_string()}</li> })
                            .collect_view()
                    }}

                </ul>
            }
        }>

            <Showcases>

                <h1>"Buttons"</h1>

                <Showcase>
                    <div class="element">
                        <Button1 text="Button 1".to_string() />
                        <Button1 text="Button 1".to_string() disabled=true />
                    </div>
                    <div class="element">
                        <Button2 text="Button 2".to_string() />
                        <Button2 text="Button 2".to_string() disabled=true />
                    </div>
                    <div class="element">
                        <Button3 text="Button 3".to_string() />
                        <Button3 text="Button 3".to_string() disabled=true />
                    </div>
                    <div class="element">
                        <Button4 text="Button 4".to_string() />
                        <Button4 text="Button 4".to_string() disabled=true />
                    </div>
                </Showcase>

                <h1>"Inputs"</h1>

                <Showcase>
                    <div class="element">
                        <Label for_id="input-1".to_string()>"Input 1"</Label>
                        <Input1 id="input-1".to_string() placeholder="input1".to_string() />
                        <Label for_id="input-1-disabled".to_string()>"Input 1 disabled"</Label>
                        <Input1
                            id="input-1-disabled".to_string()
                            placeholder="input1".to_string()
                            disabled=true
                        />
                    </div>
                    <div class="element">
                        <Label for_id="input-2".to_string()>"Input 2"</Label>
                        <Input2 id="input-2".to_string() placeholder="input2".to_string() />
                        <Label for_id="input-2-disabled".to_string()>"Input 2 disabled"</Label>
                        <Input2
                            id="input-2-disabled".to_string()
                            placeholder="input2".to_string()
                            disabled=true
                        />
                    </div>
                    <div class="element">
                        <Label for_id="input-3".to_string()>"Input 3"</Label>
                        <Input3 id="input-3".to_string() placeholder="input3".to_string() />
                        <Label for_id="input-3-disabled".to_string()>"Input 3 disabled"</Label>
                        <Input3
                            id="input-3-disabled".to_string()
                            placeholder="input3".to_string()
                            disabled=true
                        />
                    </div>
                </Showcase>

                <h1>Floating Label Inputs</h1>

                <Showcase>
                    <div class="element">
                        <div class="border float-input2 border-2">
                            <label for="float-input2" class="float-input2">
                                Floating Label Input 2 asdfasdfaf
                            </label>
                            <input
                                id="float-input2"
                                type="input"
                                class="float-input2"
                                placeholder="float-input2"
                                required=""
                            />
                        </div>
                    </div>
                    <div class="element">
                        <div class="border float-input3">
                            <label for="float-input3" class="float-input3">
                                Floating Label Input 3 asdfasdfaf
                            </label>
                            <input
                                id="float-input3"
                                type="input"
                                class="float-input3"
                                placeholder="float-input3"
                                required=""
                            />
                        </div>
                    </div>
                    <div class="element">
                        <div class="border float-input4">
                            <label for="float-input4" class="float-input4">
                                Floating Label Input 4 asdfasdfaf
                            </label>
                            <input
                                id="float-input4"
                                type="input"
                                class="float-input4"
                                placeholder="float-input4"
                                required=""
                            />
                        </div>
                    </div>
                    <div class="element">
                        <div class="border float-input5">
                            <label for="float-input5" class="float-input5">
                                Floating Label Input 5 asdfasdfaf
                            </label>
                            <input
                                id="float-input5"
                                type="input"
                                class="float-input5"
                                placeholder="float-input5"
                                required=""
                            />
                        </div>
                    </div>
                    <div class="element">
                        <div class="border float-input6">
                            <label for="float-input6" class="float-input6">
                                Floating Label Input 6 asdfasdfaf
                            </label>
                            <input
                                id="float-input6"
                                type="input"
                                class="float-input6"
                                placeholder="float-input6"
                                required=""
                            />
                        </div>
                    </div>
                </Showcase>

                <h1>"Login forms"</h1>

                <Showcase>
                    <div class="element">
                        <form class="login-form1">
                            <div class="login-title">Welcome</div>
                            <div class="form-line">
                                <Label for_id="email".to_string()>Email</Label>
                                <Input1
                                    id="email".to_string()
                                    placeholder="john@gmail.com".to_string()
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line">
                                <Label for_id="password".to_string()>Password</Label>
                                <Input1
                                    id="password".to_string()
                                    placeholder="password".to_string()
                                    {..}
                                    type="password"
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line">
                                <Button2 text="Login".to_string() />
                            </div>
                        </form>
                    </div>
                    <div class="element">
                        <form class="border-2">
                            <div class="login-title">Welcome</div>
                            <div class="form-line">
                                <Label for_id="email".to_string()>Email</Label>
                                <Input1
                                    id="email".to_string()
                                    placeholder="john@gmail.com".to_string()
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line">
                                <Label for_id="password".to_string()>Password</Label>
                                <Input1
                                    id="password".to_string()
                                    placeholder="password".to_string()
                                    attr::type="password"
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line">
                                <Button2 text="Login".to_string() />
                            </div>
                        </form>
                    </div>
                    <div class="element">
                        <form class="border-3">
                            <div class="login-title">Welcome</div>
                            <div class="form-line">
                                <Label for_id="email".to_string()>Email</Label>
                                <Input2
                                    id="email".to_string()
                                    placeholder="john@gmail.com".to_string()
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line">
                                <Label for_id="password".to_string()>Password</Label>
                                <Input2
                                    id="password".to_string()
                                    placeholder="password".to_string()
                                    attr::type="password"
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line">
                                <Button3 text="Login".to_string() />
                            </div>
                        </form>
                    </div>
                    <div class="element">
                        <form class="border-4">
                            <div class="login-title">Welcome</div>
                            <div class="form-line">
                                <Label for_id="email".to_string()>Email</Label>
                                <Input3
                                    id="email".to_string()
                                    placeholder="john@gmail.com".to_string()
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line">
                                <Label for_id="password".to_string()>Password</Label>
                                <Input3
                                    id="password".to_string()
                                    placeholder="password".to_string()
                                    attr::type="password"
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line">
                                <Button4 text="Login".to_string() />
                            </div>
                        </form>
                    </div>
                </Showcase>

                <h1>Check boxes</h1>

                <Showcase>
                    <div class="element">
                        <Checkbox1 id="checkbox-style1-1".to_string()>
                            "Checkbox 1"
                        </Checkbox1>
                        <Checkbox1 id="checkbox-style1-2".to_string() disabled=true>
                            "Disabled Checkbox 2"
                        </Checkbox1>
                    </div>
                    <div class="element">
                        <Checkbox2 id="checkbox-style2-1".to_string()>
                            "Checkbox 1"
                        </Checkbox2>
                        <Checkbox2 id="checkbox-style2-2".to_string() disabled=true>
                            "Disabled Checkbox 2"
                        </Checkbox2>
                    </div>
                    <div class="element">
                        <Checkbox3 id="checkbox-style3-1".to_string()>
                            "Checkbox 1"
                        </Checkbox3>
                        <Checkbox3 id="checkbox-style3-2".to_string() disabled=true>
                            "Disabled Checkbox 2"
                        </Checkbox3>
                    </div>
                </Showcase>

                <h1>Radio buttons</h1>

                <Showcase>
                    <div class="element">
                        <Radio1 id="radio-style1-1".to_string() name="radio-group-1".to_string()>
                            "Radio 1"
                        </Radio1>
                        <Radio1 id="radio-style1-2".to_string() name="radio-group-1".to_string()>
                            "Radio 2"
                        </Radio1>
                        <Radio1
                            id="radio-style1-3".to_string()
                            name="radio-group-1".to_string()
                            disabled=true
                        >
                            "Disabled Radio 3"
                        </Radio1>
                    </div>
                    <div class="element">
                        <Radio2 id="radio-style2-1".to_string() name="radio-group-2".to_string()>
                            "Radio 1"
                        </Radio2>
                        <Radio2 id="radio-style2-2".to_string() name="radio-group-2".to_string()>
                            "Radio 2"
                        </Radio2>
                        <Radio2
                            id="radio-style1-3".to_string()
                            name="radio-group-2".to_string()
                            disabled=true
                        >
                            "Disabled Radio 3"
                        </Radio2>
                    </div>
                    <div class="element">
                        <Radio3 id="radio-style3-1".to_string() name="radio-group-3".to_string()>
                            "Radio 1"
                        </Radio3>
                        <Radio3 id="radio-style3-2".to_string() name="radio-group-3".to_string()>
                            "Radio 2"
                        </Radio3>
                        <Radio3
                            id="radio-style1-3".to_string()
                            name="radio-group-3".to_string()
                            disabled=true
                        >
                            "Disabled Radio 3"
                        </Radio3>
                    </div>
                </Showcase>

                <h1>Sidebars</h1>

                <Showcase>
                    <div class="element">
                        <iframe
                            src="/sidebar1"
                            class="width-50 height-800 border-1 padding-1em"
                        ></iframe>
                    </div>
                </Showcase>
            </Showcases>
        </ErrorBoundary>
    }
}

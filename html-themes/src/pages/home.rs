use crate::base::BASE_URL;
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
use crate::components::showcase::{Element, Showcase, Showcases};
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
                    <Element>
                        <Button1 text="Button 1".to_string() />
                        <Button1 text="Button 1".to_string() disabled=true />
                    </Element>
                    <Element>
                        <Button2 text="Button 2".to_string() />
                        <Button2 text="Button 2".to_string() disabled=true />
                    </Element>
                    <Element>
                        <Button3 text="Button 3".to_string() />
                        <Button3 text="Button 3".to_string() disabled=true />
                    </Element>
                    <Element>
                        <Button4 text="Button 4".to_string() />
                        <Button4 text="Button 4".to_string() disabled=true />
                    </Element>
                </Showcase>

                <h1>"Inputs"</h1>

                <Showcase>
                    <Element>
                        <Label for_id="input-1".to_string()>"Input 1"</Label>
                        <Input1 id="input-1".to_string() placeholder="input1".to_string() />
                        <Label
                            for_id="input-1-disabled".to_string()
                            attr:class="text-primary-disabled"
                        >
                            "Input 1 disabled"
                        </Label>
                        <Input1
                            id="input-1-disabled".to_string()
                            placeholder="input1".to_string()
                            disabled=true
                        />
                    </Element>
                    <Element>
                        <Label for_id="input-2".to_string()>"Input 2"</Label>
                        <Input2 id="input-2".to_string() placeholder="input2".to_string() />
                        <Label
                            for_id="input-2-disabled".to_string()
                            attr:class="text-primary-disabled"
                        >
                            "Input 2 disabled"
                        </Label>
                        <Input2
                            id="input-2-disabled".to_string()
                            placeholder="input2".to_string()
                            disabled=true
                        />
                    </Element>
                    <Element>
                        <Label for_id="input-3".to_string()>"Input 3"</Label>
                        <Input3 id="input-3".to_string() placeholder="input3".to_string() />
                        <Label
                            for_id="input-3-disabled".to_string()
                            attr:class="text-primary-disabled"
                        >
                            "Input 3 disabled"
                        </Label>
                        <Input3
                            id="input-3-disabled".to_string()
                            placeholder="input3".to_string()
                            disabled=true
                        />
                    </Element>
                </Showcase>

                <h1>"Login forms"</h1>

                <Showcase>
                    <Element>
                        <form class="border-3 border-secondary p-4 flex gap-4 flex-col">
                            <div class="text-2xl mb-4 text-center">Welcome</div>
                            <div class="form-line">
                                <Label for_id="email".to_string() class:block>
                                    Email
                                </Label>
                                <Input1
                                    id="email".to_string()
                                    placeholder="john@gmail.com".to_string()
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line">
                                <Label for_id="password".to_string() class:block>
                                    Password
                                </Label>
                                <Input1
                                    id="password".to_string()
                                    placeholder="password".to_string()
                                    {..}
                                    type="password"
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line text-right">
                                <Button2 text="Login".to_string() />
                            </div>
                        </form>
                    </Element>
                    <Element>
                        <form class="border2 border-secondary p-4 flex gap-4 flex-col">
                            <div class="text-2xl mb-4 text-center">Welcome</div>
                            <div class="form-line">
                                <Label for_id="email".to_string() class:block>
                                    Email
                                </Label>
                                <Input1
                                    id="email".to_string()
                                    placeholder="john@gmail.com".to_string()
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line">
                                <Label for_id="password".to_string() class:block>
                                    Password
                                </Label>
                                <Input1
                                    id="password".to_string()
                                    placeholder="password".to_string()
                                    {..}
                                    type="password"
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line text-right">
                                <Button2 text="Login".to_string() />
                            </div>
                        </form>
                    </Element>
                    <Element>
                        <form class="border3 border-secondary p-4 flex gap-4 flex-col">
                            <div class="text-2xl mb-4 text-center">Welcome</div>
                            <div class="form-line">
                                <Label for_id="email".to_string() class:block>
                                    Email
                                </Label>
                                <Input2
                                    id="email".to_string()
                                    placeholder="john@gmail.com".to_string()
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line">
                                <Label for_id="password".to_string() class:block>
                                    Password
                                </Label>
                                <Input2
                                    id="password".to_string()
                                    placeholder="password".to_string()
                                    {..}
                                    type="password"
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line text-right">
                                <Button3 text="Login".to_string() />
                            </div>
                        </form>
                    </Element>
                    <Element>
                        <form class="border4 border-secondary p-4 flex gap-4 flex-col">
                            <div class="text-2xl mb-4 text-center">Welcome</div>
                            <div class="form-line">
                                <Label for_id="email".to_string() class:block>
                                    Email
                                </Label>
                                <Input3
                                    id="email".to_string()
                                    placeholder="john@gmail.com".to_string()
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line">
                                <Label for_id="password".to_string() class:block>
                                    Password
                                </Label>
                                <Input3
                                    id="password".to_string()
                                    placeholder="password".to_string()
                                    {..}
                                    type="password"
                                    class:padding-tb-0-4
                                    class:width-100
                                />
                            </div>
                            <div class="form-line text-right">
                                <Button4 text="Login".to_string() />
                            </div>
                        </form>
                    </Element>
                </Showcase>

                <h1>Check boxes</h1>

                <Showcase>
                    <Element>
                        <Checkbox1 id="checkbox-style1-1".to_string()>"Checkbox 1"</Checkbox1>
                        <Checkbox1 id="checkbox-style1-2".to_string() disabled=true>
                            "Disabled Checkbox 2"
                        </Checkbox1>
                    </Element>
                    <Element>
                        <Checkbox2 id="checkbox-style2-1".to_string()>"Checkbox 1"</Checkbox2>
                        <Checkbox2 id="checkbox-style2-2".to_string() disabled=true>
                            "Disabled Checkbox 2"
                        </Checkbox2>
                    </Element>
                    <Element>
                        <Checkbox3 id="checkbox-style3-1".to_string()>"Checkbox 1"</Checkbox3>
                        <Checkbox3 id="checkbox-style3-2".to_string() disabled=true>
                            "Disabled Checkbox 2"
                        </Checkbox3>
                    </Element>
                </Showcase>

                <h1>Radio buttons</h1>

                <Showcase>
                    <Element>
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
                    </Element>
                    <Element>
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
                    </Element>
                    <Element>
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
                    </Element>
                </Showcase>

                <h1>Sidebars</h1>

                <Showcase>
                    <Element>
                        <iframe
                            src=format!("{}/sidebar1", BASE_URL)
                            class="w-200 h-100 border-1 padding-1em"
                        ></iframe>
                    </Element>

                    <Element>
                        <iframe
                            src=format!("{}/sidebar2", BASE_URL)
                            class="w-200 h-100 border-1 padding-1em"
                        ></iframe>
                    </Element>

                    <Element>
                        <iframe
                            src=format!("{}/sidebar3", BASE_URL)
                            class="w-200 h-100 border-1 padding-1em"
                        ></iframe>
                    </Element>

                    <Element>
                        <iframe
                            src=format!("{}/sidebar4", BASE_URL)
                            class="w-200 h-100 border-1 padding-1em"
                        ></iframe>
                    </Element>

                    <Element>
                        <iframe
                            src=format!("{}/sidebar5", BASE_URL)
                            class="w-200 h-100 border-1 padding-1em"
                        ></iframe>
                    </Element>
                    <Element>
                        <iframe
                            src=format!("{}/sidebar6", BASE_URL)
                            class="w-200 h-100 border-1 padding-1em"
                        ></iframe>
                    </Element>
                </Showcase>
            </Showcases>
        </ErrorBoundary>
    }
}

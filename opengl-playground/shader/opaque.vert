#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec3 aColor;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec3 Normal;
out vec3 FragPos;
out vec3 Color;
out vec3 HoloPos;

void main()
{
    gl_Position = projection * view * model * vec4(aPos, 1.0);
    // This is not correct normal computation!!!
    Normal =  normalize(vec3(transpose(inverse(model)) * vec4(aNormal, 1.0)));
    FragPos = vec3(model * vec4(aPos, 1.0));
    Color = aColor;
    HoloPos = aPos;
}

#version 330 core

layout (location = 0) in vec3 aPos;

uniform mat4 transform;
uniform mat4 rotation;
uniform mat4 projection;

out vec4 color;

void main()
{
    gl_Position = projection * transform * rotation * vec4(aPos, 1.0);
    color = vec4(1.0f, 0f, 1.0f, 1.0f);
}

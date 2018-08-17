#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;

uniform mat4 transform;
uniform mat4 rotation;
uniform mat4 projection;

out vec3 Normal;
out vec3 FragPos;

void main()
{
    gl_Position = projection * transform * rotation * vec4(aPos, 1.0);
    // This is not correct normal computation!!!
    Normal = vec3(projection * transform * rotation * vec4(aNormal, 1.0));
    FragPos = vec3(transform * rotation * vec4(aPos, 1.0));
}

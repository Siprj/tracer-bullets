#version 330 core

in vec3 Normal;
in vec3 FragPos;

out vec4 FragColor;

uniform vec3 lightPos;

const float ambientStrength = 0.1f;
const vec3 lightColour = vec3(0.774f, 0.439f, 0.164f);

void main()
{
    vec3 norm = normalize(Normal);
    vec3 lightDir = normalize(lightPos);
    float diff = max(dot(norm, lightDir), 0.0f);

    vec3 res = lightColour * (ambientStrength + diff);
    FragColor = vec4(res, 1.0f);
}


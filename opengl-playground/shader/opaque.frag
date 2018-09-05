#version 330 core

in vec3 Normal;
in vec3 FragPos;
in vec3 Color;

out vec4 FragColor;

uniform vec3 lightPos;
uniform vec3 viewPos;

const float ambientStrength = 0.2f;
const float specularStrength = 0.5;
const vec3 lightColour = vec3(1.0f, 1.0f, 1.0f);

void main()
{
    vec3 ambient = lightColour * ambientStrength;

    vec3 lightDir = normalize(lightPos - FragPos);
    vec3 diff = max(dot(Normal, lightDir), 0.0f) * lightColour;

    vec3 viewDir = normalize(viewPos - FragPos);
    vec3 reflectDir = reflect(-lightDir, Normal);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), 64);
    vec3 specular = specularStrength * spec * lightColour;

    vec3 res = Color * (specular + ambient + diff);
    FragColor = vec4(res, 1.0f);
}


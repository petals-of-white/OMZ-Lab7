#version 330 core

layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 texCoord;
uniform mat4 u_transform;
uniform mat4 u_rotate;
uniform float y;
out vec3 TexCoord;

void main()
{
    vec4 rotated = u_rotate * vec4(aPos.x, y, aPos.y, 1.0);

    gl_Position = u_transform * vec4(aPos.r, y, aPos.g, 1.0);
    
    TexCoord = ((rotated + 1.0) / 2.0).xyz;
}